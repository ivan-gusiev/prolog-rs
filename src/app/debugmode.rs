extern crate crossterm;
extern crate tui;

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

use crate::Machine;

struct App<'a> {
    flags_state: TableState,
    regs_state: TableState,
    code_state: TableState,
    data_state: TableState,
    machine: &'a mut Machine,
}

impl<'a> App<'a> {
    fn new(machine: &'a mut Machine) -> App<'a> {
        App {
            flags_state: TableState::default(),
            regs_state: TableState::default(),
            code_state: TableState::default(),
            data_state: TableState::default(),
            machine,
        }
    }

    fn flags(&self) -> Vec<(&'a str, String)> {
        vec![
            ("H", self.machine.get_h().to_string()),
            ("S", self.machine.get_s().to_string()),
            ("P", self.machine.get_p().to_string()),
            ("Mode", self.machine.get_mode().to_string()),
            ("Fail", self.machine.get_fail().to_string()),
        ]
    }

    fn regs(&self) -> Vec<(String, String)> {
        self.machine
            .iter_reg()
            .enumerate()
            .map(|(i, val)| (format!("X{} =", i), val.to_string()))
            .collect()
    }

    fn next_instruction(&mut self) {
        _ = self.machine.step();
    }
}

fn run_app<B: Backend>(terminal: &mut Terminal<B>, mut app: App) -> io::Result<()> {
    loop {
        terminal.draw(|f| ui(f, &mut app))?;

        if let Event::Key(key) = event::read()? {
            match key.code {
                KeyCode::Char('q') => return Ok(()),
                KeyCode::Char(' ') => app.next_instruction(),
                _ => {}
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
    let rows = app.regs().into_iter().map(|(name, value)| {
        let height = 1;
        let cells = vec![Cell::from(name).style(name_style), Cell::from(value)];
        Row::new(cells).height(height as u16)
    });
    Table::new(rows)
        .block(Block::default().borders(Borders::ALL).title("Registers"))
        .widths(&[Constraint::Length(5), Constraint::Min(10)])
}

fn render_code<'a, 'b>(app: &App<'b>) -> Table<'a>
where
    'b: 'a,
{
    let selected_style = Style::default().add_modifier(Modifier::REVERSED);
    let name_style = Style::default().fg(Color::White);
    let rows = app
        .machine
        .get_code()
        .into_iter()
        .enumerate()
        .map(|(i, instr)| {
            let height = 1;
            let cells = vec![
                Cell::from(format!("{:03}", i)).style(name_style),
                Cell::from(instr.to_string()),
            ];
            Row::new(cells).height(height as u16)
        });
    Table::new(rows)
        .highlight_style(selected_style)
        .highlight_symbol("*")
        .block(Block::default().borders(Borders::ALL).title("Code"))
        .widths(&[Constraint::Length(3), Constraint::Percentage(100)])
}

fn render_data<'a, 'b>(app: &App<'b>) -> Table<'a>
where
    'b: 'a,
{
    let name_style = Style::default().fg(Color::White);
    let rows = app.machine.iter_heap().enumerate().map(|(i, instr)| {
        let height = 1;
        let cells = vec![
            Cell::from(format!("{:03}", i)).style(name_style),
            Cell::from(instr.to_string()),
        ];
        Row::new(cells).height(height as u16)
    });
    Table::new(rows)
        .block(Block::default().borders(Borders::ALL).title("Data"))
        .widths(&[Constraint::Length(3), Constraint::Percentage(100)])
}

fn render_help<'a>() -> Paragraph<'a> {
    const TEXT: &'static str = r#"
 q      quit
 SPACE  execute instruction"#;
    Paragraph::new(TEXT).block(Block::default().borders(Borders::ALL).title("Help"))
}

fn ui<B: Backend>(f: &mut Frame<B>, app: &mut App) {
    let layout = calculate_layout(f);

    app.code_state.select(Some(app.machine.get_p().into()));

    f.render_stateful_widget(render_flags(app), layout.flags, &mut app.flags_state);
    f.render_stateful_widget(
        render_regs(app, layout.regs),
        layout.regs,
        &mut app.regs_state,
    );
    f.render_stateful_widget(render_code(app), layout.code, &mut app.code_state);
    f.render_stateful_widget(render_data(app), layout.data, &mut app.data_state);
    f.render_widget(render_help(), layout.footer);
}

pub fn start_debugmode<'a>(machine: &'a mut Machine) -> Result<(), Box<dyn Error>> {
    // setup terminal
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    // create app and run it
    let app = App::new(machine);
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
        println!("{:?}", err)
    }

    Ok(())
}
