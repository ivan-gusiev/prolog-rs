extern crate crossterm;
extern crate tui;

use prolog_rs::{
    data::{CodePtr, Data, FramePtr, HeapPtr, RegPtr, Str},
    symbol::{to_display, SymDisplay},
    util::collapse,
};

use self::crossterm::{
    event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use self::tui::{
    backend::{Backend, CrosstermBackend},
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    widgets::{Block, Borders, Cell, Paragraph, Row, Table, TableState},
    Frame, Terminal,
};
use std::{error::Error, io, iter};

use crate::PrologApp;

enum InputState {
    Default,
    CodeNavigator,
    FlagEditor,
    HeapNavigator,
    Constructor,
}

impl InputState {
    fn help_text(&self) -> &'static str {
        const DEFAULT_TEXT: &str = r#"
    q      quit                  b      bind variables
    c      navigate code
    h      navigate heap
    f      toggle flags
    SPACE  execute instruction"#;

        const CODE_NAVIGATION_TEXT: &str = r#"
    Navigating Code >>>
        q      back to debug
        UP     move up
        DOWN   move down
        SPACE  set code pointer"#;

        const FLAG_EDITOR_TEXT: &str = r#"
    Editing Flags >>>
        q      back to debug
        f      toggle fail
        h      toggle halt"#;

        const DATA_NAVIGATOR_TEXT: &str = r#"
    Navigating Heap >>>
        q      back to debug
        UP     move up
        DOWN   move down
        c      construct terms"#;

        const CONSTRUCTOR_TEXT: &str = r#"
    Constructor Output >>>
        {}

        q      back to data
        UP     move up
        DOWN   move down"#;

        match self {
            InputState::Default => DEFAULT_TEXT,
            InputState::CodeNavigator => CODE_NAVIGATION_TEXT,
            InputState::FlagEditor => FLAG_EDITOR_TEXT,
            InputState::HeapNavigator => DATA_NAVIGATOR_TEXT,
            InputState::Constructor => CONSTRUCTOR_TEXT,
        }
    }
}

struct App<'a> {
    flags_state: TableState,
    regs_state: TableState,
    code_state: TableState,
    data_state: TableState,
    input_state: InputState,
    prolog: &'a mut PrologApp,
}

impl<'a> App<'a> {
    fn new(machine: &'a mut PrologApp) -> App<'a> {
        App {
            flags_state: TableState::default(),
            regs_state: TableState::default(),
            code_state: TableState::default(),
            data_state: TableState::default(),
            input_state: InputState::Default,
            prolog: machine,
        }
    }

    fn flags(&self) -> Vec<(&'a str, String)> {
        let machine = &self.prolog.machine;
        vec![
            ("H", machine.get_h().to_string()),
            ("S", machine.get_s().to_string()),
            ("P", machine.get_p().to_string()),
            ("CP", machine.get_cp().to_string()),
            ("Mode", machine.get_mode().to_string()),
            ("Fail", machine.get_fail().to_string()),
            ("Halt", machine.get_halt().to_string()),
        ]
    }

    fn regs(&self) -> Vec<(String, String, String)> {
        let st = &self.prolog.symbol_table;
        let annotate_reg = |reg: RegPtr| {
            let mut annotations = Vec::<String>::new();
            if let Ok(value) = self
                .prolog
                .machine
                .deref(reg.into())
                .and_then(|addr| self.prolog.machine.get_store(addr))
            {
                match value {
                    Data::Functor(f) => {
                        annotations.push(format!("{}", to_display(&f, st)));
                    }
                    Data::Str(Str(f_ptr)) => {
                        if let Data::Functor(f) = self.prolog.machine.get_heap(f_ptr) {
                            annotations.push(format!("{}", to_display(&f, st)));
                        }
                    }
                    _ => (),
                }
            }
            annotations.join(", ")
        };

        self.prolog
            .machine
            .iter_reg()
            .map(|(reg, val)| (reg.to_string(), val.sym_to_str(st), annotate_reg(reg)))
            .collect()
    }

    fn stack(&'a self) -> Vec<String> {
        let st = &self.prolog.symbol_table;
        let annotate = |stack: &FramePtr| -> String {
            let mut annotations = Vec::<String>::new();
            if let Some(q) = self.prolog.query.as_ref() {
                if let Some(x) = q.var_mapping.get(&stack.into()) {
                    annotations.push(format!("q.{}", to_display(&x, st)))
                }
            }
            if let Some(p) = self.prolog.program.as_ref() {
                if let Some(x) = p.get(&stack.into()) {
                    annotations.push(format!("p.{}", x.sym_to_str(st)))
                }
            }
            annotations.join(", ")
        };

        self.prolog
            .machine
            .walk_stack()
            .enumerate()
            .flat_map(|(depth, stack_result)| match stack_result {
                Ok(stack) => {
                    iter::once(format!("depth #{depth}: CP = {}", stack.cp))
                        .chain(stack.iter_var().map(|(ptr, data)| {
                            format!("Y{} = {} ({})", ptr.0, data, annotate(&ptr))
                        }))
                        .collect::<Vec<_>>()
                }
                Err(e) => {
                    vec![format!("stack walk error: {e}")]
                }
            })
            .collect()
    }

    fn bindings(&self) -> Vec<(String, String, String)> {
        let st = &self.prolog.symbol_table;

        self.prolog
            .machine
            .vars()
            .iter()
            .map(|rec| {
                (
                    rec.variable.sym_to_str(st),
                    rec.mapping.to_string(),
                    rec.address.to_string(),
                )
            })
            .collect()
    }

    fn next_instruction(&mut self) {
        _ = self.prolog.machine.step();
    }

    fn bind_variables(&mut self) {
        _ = self.prolog.machine.bind_vars();
    }
}

fn inc_mod(x: usize, len: usize) -> usize {
    if x >= len - 1 {
        0
    } else {
        x + 1
    }
}

fn dec_mod(x: usize, len: usize) -> usize {
    if x == 0 {
        len - 1
    } else {
        x - 1
    }
}

fn run_app<B: Backend>(terminal: &mut Terminal<B>, mut app: App) -> io::Result<()> {
    loop {
        terminal.draw(|f| ui(f, &mut app))?;

        if let Event::Key(key) = event::read()? {
            match app.input_state {
                InputState::Default => match key.code {
                    KeyCode::Char('q') => return Ok(()),
                    KeyCode::Char(' ') => app.next_instruction(),
                    KeyCode::Char('b') => app.bind_variables(),
                    KeyCode::Char('c') => {
                        app.code_state
                            .select(Some(app.prolog.machine.get_p().into()));
                        app.input_state = InputState::CodeNavigator;
                    }
                    KeyCode::Char('h') => {
                        app.data_state.select(Some(0));
                        app.input_state = InputState::HeapNavigator;
                    }
                    KeyCode::Char('f') => {
                        app.input_state = InputState::FlagEditor;
                    }
                    _ => {}
                },
                InputState::CodeNavigator => match key.code {
                    KeyCode::Char('q') => {
                        app.code_state.select(None);
                        app.input_state = InputState::Default;
                    }
                    KeyCode::Up => {
                        if let Some(x) = app.code_state.selected() {
                            app.code_state
                                .select(Some(dec_mod(x, app.prolog.machine.code_len())))
                        }
                    }
                    KeyCode::Down => {
                        if let Some(x) = app.code_state.selected() {
                            app.code_state
                                .select(Some(inc_mod(x, app.prolog.machine.code_len())))
                        }
                    }
                    KeyCode::Char(' ') => {
                        if let Some(x) = app.code_state.selected() {
                            app.prolog.machine.set_p(CodePtr(x))
                        }
                    }
                    _ => {}
                },
                InputState::FlagEditor => match key.code {
                    KeyCode::Char('q') => {
                        app.input_state = InputState::Default;
                    }
                    KeyCode::Char('f') => {
                        app.prolog.machine.set_fail(!app.prolog.machine.get_fail())
                    }
                    KeyCode::Char('h') => {
                        app.prolog.machine.set_halt(!app.prolog.machine.get_halt())
                    }
                    _ => {}
                },
                InputState::HeapNavigator => match key.code {
                    KeyCode::Char('q') => {
                        app.data_state.select(None);
                        app.input_state = InputState::Default;
                    }
                    KeyCode::Up => {
                        if let Some(x) = app.data_state.selected() {
                            app.data_state
                                .select(Some(dec_mod(x, app.prolog.machine.heap_len())))
                        }
                    }
                    KeyCode::Down => {
                        if let Some(x) = app.data_state.selected() {
                            app.data_state
                                .select(Some(inc_mod(x, app.prolog.machine.heap_len())))
                        }
                    }
                    KeyCode::Char('d') => {
                        if app.data_state.selected().is_some() {
                            app.input_state = InputState::Constructor;
                        }
                    }
                    _ => {}
                },
                InputState::Constructor => match key.code {
                    KeyCode::Char('q') => {
                        app.input_state = InputState::HeapNavigator;
                    }
                    KeyCode::Up => {
                        if let Some(x) = app.data_state.selected() {
                            app.data_state
                                .select(Some(dec_mod(x, app.prolog.machine.heap_len())))
                        }
                    }
                    KeyCode::Down => {
                        if let Some(x) = app.data_state.selected() {
                            app.data_state
                                .select(Some(inc_mod(x, app.prolog.machine.heap_len())))
                        }
                    }
                    _ => {}
                },
            }
        }
    }
}

struct Windows {
    flags: Rect,
    regs: Rect,
    code: Rect,
    bindings: Rect,
    heap: Rect,
    stack: Rect,
    footer: Rect,
}

fn calculate_layout<B: Backend>(f: &mut Frame<B>) -> Windows {
    let rows = Layout::default()
        .constraints(
            [
                Constraint::Length(14),
                Constraint::Min(0),
                Constraint::Length(10),
            ]
            .as_ref(),
        )
        .margin(1)
        .split(f.size());

    let &[header, main, footer] = rows.as_slice() else { panic!("could not find the layout") };

    let header_rows = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Length(20), Constraint::Min(20)].as_ref())
        .split(header);

    let &[flags, regs] = header_rows.as_slice() else { panic!("could not find the layout") };

    let main_rows = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(50), Constraint::Percentage(50)].as_ref())
        .split(main);

    let &[codepane, data] = main_rows.as_slice() else { panic!("could not find the layout") };

    let code_rows = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Percentage(50), Constraint::Percentage(50)].as_ref())
        .split(codepane);

    let &[code, bindings] = code_rows.as_slice() else { panic!("could not find the layout") };

    let data_rows = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Percentage(50), Constraint::Percentage(50)].as_ref())
        .split(data);

    let &[heap, stack] = data_rows.as_slice() else { panic!("could not find the layout") };

    Windows {
        flags,
        regs,
        code,
        bindings,
        heap,
        stack,
        footer,
    }
}

fn render_flags<'a, 'b>(app: &App<'b>) -> Table<'a>
where
    'b: 'a,
{
    let name_style = Style::default().fg(Color::White);
    let rows = app.flags().into_iter().map(|(name, value)| {
        let height = 1;
        let cells = vec![Cell::from(name).style(name_style), Cell::from(value)];
        Row::new(cells).height(height as u16)
    });
    Table::new(rows)
        .block(Block::default().borders(Borders::ALL).title("Flags"))
        .widths(&[Constraint::Min(5), Constraint::Min(10)])
}

fn render_regs<'a, 'b>(app: &App<'b>, _size: Rect) -> Table<'a>
where
    'b: 'a,
{
    let name_style = Style::default().fg(Color::White);
    let rows = app.regs().into_iter().map(|(name, value, ann)| {
        let height = 1;
        let cells = vec![
            Cell::from(name).style(name_style),
            Cell::from(value),
            Cell::from(ann),
        ];
        Row::new(cells).height(height as u16)
    });
    Table::new(rows)
        .block(Block::default().borders(Borders::ALL).title("Registers"))
        .widths(&[
            Constraint::Length(5),
            Constraint::Percentage(50),
            Constraint::Percentage(50),
        ])
}

fn render_code<'a, 'b>(app: &App<'b>) -> Table<'a>
where
    'b: 'a,
{
    let selected_style = Style::default().add_modifier(Modifier::REVERSED);
    let name_style = Style::default().fg(Color::White);
    let st = &app.prolog.symbol_table;
    let rows = app
        .prolog
        .machine
        .get_code()
        .into_iter()
        .enumerate()
        .map(|(i, instr)| {
            let cells = vec![
                Cell::from(if app.prolog.machine.get_p().0 == i {
                    ">"
                } else {
                    ""
                }),
                Cell::from(format!("{i:03}")).style(name_style),
                Cell::from(instr.sym_to_str(st)),
            ];
            Row::new(cells).height(1)
        });
    Table::new(rows)
        .highlight_style(selected_style)
        .block(Block::default().borders(Borders::ALL).title("Code"))
        .widths(&[
            Constraint::Length(1),
            Constraint::Length(3),
            Constraint::Percentage(100),
        ])
}

fn render_bindings<'a, 'b>(app: &App<'b>) -> Table<'a>
where
    'b: 'a,
{
    let selected_style = Style::default().add_modifier(Modifier::REVERSED);
    let name_style = Style::default().fg(Color::White);
    let rows = app
        .bindings()
        .into_iter()
        .map(|(var_name, local, heap_ptr)| {
            let cells = vec![
                Cell::from(var_name).style(name_style),
                Cell::from(local),
                Cell::from(heap_ptr),
            ];
            Row::new(cells).height(1)
        });
    Table::new(rows)
        .highlight_style(selected_style)
        .block(
            Block::default()
                .borders(Borders::ALL)
                .title("Query Bindings"),
        )
        .widths(&[
            Constraint::Percentage(50),
            Constraint::Percentage(25),
            Constraint::Percentage(25),
        ])
}

fn render_heap<'a, 'b>(app: &App<'b>) -> Table<'a>
where
    'b: 'a,
{
    let selected_style = Style::default().add_modifier(Modifier::REVERSED);
    let name_style = Style::default().fg(Color::White);
    let rows = app.prolog.machine.iter_heap().enumerate().map(|(i, data)| {
        let height = 1;
        let cells = vec![
            Cell::from(format!("{i:03}")).style(name_style),
            Cell::from(data.sym_to_str(&app.prolog.symbol_table)),
        ];
        Row::new(cells).height(height as u16)
    });
    Table::new(rows)
        .highlight_style(selected_style)
        .block(Block::default().borders(Borders::ALL).title("Heap"))
        .widths(&[Constraint::Length(3), Constraint::Percentage(100)])
}

fn render_stack<'a>(app: &App<'a>) -> Table<'a> {
    let rows = app.stack().into_iter().map(|str| {
        let cells = vec![Cell::from(str)];
        Row::new(cells).height(1)
    });
    Table::new(rows)
        .block(Block::default().borders(Borders::ALL).title("Stack"))
        .widths(&[Constraint::Percentage(100)])
}

fn render_help<'a>(app: &mut App) -> Paragraph<'a> {
    let text = match app.input_state {
        InputState::Constructor => {
            let heap_len = app.prolog.machine.heap_len();
            let heap_ptr = HeapPtr(dec_mod(
                inc_mod(app.data_state.selected().unwrap_or(0), heap_len),
                heap_len,
            ));
            let construct_result = app.prolog.machine.construct_term(
                heap_ptr.into(),
                &app.prolog.query_variables,
                &mut app.prolog.symbol_table,
            );

            let construct = construct_result
                .map(|t| t.sym_to_str(&app.prolog.symbol_table))
                .map_err(|err| err.to_string());

            InputState::Constructor
                .help_text()
                .replace("{}", collapse(construct).as_str())
        }
        _ => app.input_state.help_text().to_string(),
    };

    Paragraph::new(text).block(Block::default().borders(Borders::ALL).title("Help"))
}

fn ui<B: Backend>(f: &mut Frame<B>, app: &mut App) {
    let layout = calculate_layout(f);

    f.render_stateful_widget(render_flags(app), layout.flags, &mut app.flags_state);
    f.render_stateful_widget(
        render_regs(app, layout.regs),
        layout.regs,
        &mut app.regs_state,
    );
    f.render_stateful_widget(render_code(app), layout.code, &mut app.code_state);
    f.render_stateful_widget(render_bindings(app), layout.bindings, &mut app.code_state);
    f.render_stateful_widget(render_heap(app), layout.heap, &mut app.data_state);
    f.render_stateful_widget(render_stack(app), layout.stack, &mut app.code_state);
    f.render_widget(render_help(app), layout.footer);
}

pub fn start_debugmode(prolog: &mut PrologApp) -> Result<(), Box<dyn Error>> {
    // setup terminal
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    // create app and run it
    let app = App::new(prolog);
    let res = run_app(&mut terminal, app);

    // restore terminal
    disable_raw_mode()?;
    execute!(
        terminal.backend_mut(),
        LeaveAlternateScreen,
        DisableMouseCapture
    )?;
    terminal.show_cursor()?;

    if let Err(err) = res {
        println!("{err:?}")
    }

    Ok(())
}
