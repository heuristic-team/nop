use frontend::{Diagnostic, lexer::Span};

use crate::cli::Input;
use termion::{color, style};

// if we see a file which is longer than 999'999 lines, we do not care anymore
const PREFIX_LEN: usize = 6;

const UNDERLINE_COLOR: color::Green = color::Green;

fn print_prefix(line: Option<usize>) {
    if let Some(line) = line {
        // print prefix that looks like `  42 | `
        eprint!("{:>PREFIX_LEN$} | ", line);
    } else {
        // print prefix that looks like `     | `
        eprint!("{:>PREFIX_LEN$} | ", "");
    }
}

fn print_overlapping_lines(input: &str, span: Span) {
    let lines_to_show = get_line_boundaries(input)
        .enumerate()
        .zip(input.lines())
        .skip_while(|&((_, (_, e)), _)| e < span.start)
        .take_while(|&((_, (b, e)), _)| b <= span.end && e >= span.start)
        .map(|((i, boundaries), line)| (i + 1, boundaries, line));

    // loop below is used to make pretty underlines
    // examples:
    //   37 | a := foobar
    //      |      ^~~~~^
    //
    //   42 | a := foo(1,
    //      |      ^~~~~~
    //   43 |          2,
    //      |          ~~
    //   44 |          3)
    //      |          ~^

    let mut i;
    for (number, (start_offset, end_offset), line) in lines_to_show {
        print_prefix(Some(number));
        eprintln!("{}", line);

        print_prefix(None);

        i = start_offset;
        while i < span.start {
            eprint!(" ");
            i += 1;
        }

        let mut started_underlining = false;
        if i == span.start {
            eprint!("{}^", color::Fg(UNDERLINE_COLOR));
            started_underlining = true;
            i += 1;
        }

        let mut line_chars = line.chars().skip(i - start_offset);
        while i < end_offset && i < span.end {
            // `unwrap_or` is needed because underline may be under a `\n`,
            // which is removed by `.lines()`
            if line_chars.next().map(char::is_whitespace).unwrap_or(false) && !started_underlining {
                eprint!(" ");
            } else {
                eprint!("{}~", color::Fg(UNDERLINE_COLOR));
                started_underlining = true;
            }
            i += 1;
        }

        // `span.start != span.end` to verify that we don't print "^^" under
        // a single character token
        if i == span.end && span.start != span.end {
            eprint!("{}^", color::Fg(UNDERLINE_COLOR));
        }
        eprintln!("{}", color::Fg(color::Reset));
    }
}

fn get_line_boundaries<'b>(source: &'b str) -> impl Iterator<Item = (usize, usize)> {
    source
        .chars()
        .enumerate()
        .filter_map(|(i, c)| if c == '\n' { Some(i) } else { None })
        .scan(0, |i: &mut usize, l: usize| {
            let res = (*i, l);
            *i = l + 1;
            Some(res)
        })
}

fn print_with_location(line: usize, column: usize, msg: &str) {
    eprintln!("{}:{}: {}", line, column, msg);
}

fn print_msg(input: &Input, span: Span, msg: &str) {
    let (line_number, column_number) = {
        let mut line_boundaries = get_line_boundaries(&input.contents).enumerate();

        let (number, (b, _)) = line_boundaries
            .find(|&(_, (lb, le))| lb <= span.end && le >= span.start)
            .expect("expected valid span");
        (number + 1, span.start - b + 1)
    };

    print_with_location(line_number, column_number, &msg);
    print_overlapping_lines(&input.contents, span);
}

pub fn print_error(input: &Input, diagnostic: &Diagnostic) {
    eprintln!("{}{}{}:", style::Bold, input.name, style::Reset);
    print_msg(
        &input,
        diagnostic.span,
        &format!(
            "{}{}error{}: {}{}",
            style::Bold,
            color::Fg(color::Red),
            color::Fg(color::Reset),
            diagnostic.msg,
            style::Reset,
        ),
    );

    for note in &diagnostic.notes {
        print_msg(
            &input,
            note.span,
            &format!(
                "{}{}note{}: {}{}",
                style::Bold,
                color::Fg(color::Cyan),
                color::Fg(color::Reset),
                note.value,
                style::Reset,
            ),
        );
    }
}
