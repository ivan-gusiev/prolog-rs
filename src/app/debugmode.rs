extern crate crossterm;
extern crate tui;

use prolog_rs::{
    data::{CodePtr, HeapPtr, RegPtr},
    symbol::SymDisplay,
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
use std::{error::Error, io};

use crate::PrologApp;

enum InputState {
    Default,
    CodeNavigator,
    FlagEditor,
    DataNavigator,
    Decompiler,
}

impl InputState {
    fn help_text(&self) -> &'static str {
        const DEFAULT_TEXT: &str = r#"
    q      quit
    c      navigate code
    d      navigate data
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
    Navigating Data >>>
        q      back to debug
        UP     move up
        DOWN   move down
        d      decompile"#;

        const DECOMPILER_TEXT: &str = r#"
    Decompiler Output >>>
        {}

        q      back to debug"#;

        match self {
            InputState::Default => DEFAULT_TEXT,
            InputState::CodeNavigator => CODE_NAVIGATION_TEXT,
            InputState::FlagEditor => FLAG_EDITOR_TEXT,
            InputState::DataNavigator => DATA_NAVIGATOR_TEXT,
            InputState::Decompiler => DECOMPILER_TEXT,
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
            ("Mode", machine.get_mode().to_string()),
            ("Fail", machine.get_fail().to_string()),
            ("Halt", machine.get_halt().to_string()),
        ]
    }

    fn regs(&self) -> Vec<(String, String, String)> {
        let st = &self.prolog.symbol_table;
        let annotate_reg = |reg: RegPtr| {
            let mut annotations = Vec::<String>::new();
            if let Some(q) = self.prolog.query.as_ref() {
                if let Some(x) = q.var_mapping.get(&reg) {
                    annotations.push(format!("q.{}", x.sym_to_str(st)))
                }
            }
            if let Some(p) = self.prolog.program.as_ref() {
                if let Some(x) = p.get(&reg) {
                    annotations.push(format!("p.{}", x.sym_to_str(st)))
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

    fn next_instruction(&mut self) {
        _ = self.prolog.machine.step();
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
                    KeyCode::Char('c') => {
                        app.code_state
                            .select(Some(app.prolog.machine.get_p().into()));
                        app.input_state = InputState::CodeNavigator;
                    }
                    KeyCode::Char('d') => {
                        app.data_state.select(Some(0));
                        app.input_state = InputState::DataNavigator;
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
                InputState::DataNavigator => match key.code {
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
                        if let Some(_) = app.data_state.selected() {
                            app.input_state = InputState::Decompiler;
                        }
                    }
                    _ => {}
                },
                InputState::Decompiler => match key.code {
                    KeyCode::Char('q') => {
                        app.input_state = InputState::DataNavigator;
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
    data: Rect,
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

    let &[code, data] = main_rows.as_slice() else { panic!("could not find the layout") };

    Windows {
        flags,
        regs,
        code,
        data,
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

fn render_data<'a, 'b>(app: &App<'b>) -> Table<'a>
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
        .block(Block::default().borders(Borders::ALL).title("Data"))
        .widths(&[Constraint::Length(3), Constraint::Percentage(100)])
}

fn render_help<'a, 'b>(app: &mut App<'b>) -> Paragraph<'a> {
    let text = match app.input_state {
        InputState::Decompiler => {
            let heap_len = app.prolog.machine.heap_len();
            let heap_ptr = HeapPtr(dec_mod(
                inc_mod(app.data_state.selected().unwrap_or(0), heap_len),
                heap_len,
            ));
            let decompile_result = app.prolog.machine.decompile(
                heap_ptr.into(),
                &app.prolog.query_variables,
                &mut app.prolog.symbol_table,
            );

            let decompile = decompile_result
                .map(|t| t.sym_to_str(&app.prolog.symbol_table))
                .map_err(|err| err.to_string());

            InputState::Decompiler
                .help_text()
                .replace("{}", collapse(decompile).as_str())
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
    f.render_stateful_widget(render_data(app), layout.data, &mut app.data_state);
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
