use frontend::{Diagnostic, lexer::Span};

use crate::cli::Input;

const PREFIX_LEN: usize = 5;

fn print_prefix(line: Option<usize>) {
    if let Some(line) = line {
        eprint!("{:>PREFIX_LEN$} | ", line);
    } else {
        eprint!("{:>PREFIX_LEN$} | ", "");
    }
}

fn print_overlapping_lines(input: &str, span: Span) {
    let lines_to_show //: impl Iterator<(usize, &str)>
    = get_line_boundaries(input)
        .enumerate()
        .zip(input.lines())
        .skip_while(|&((_, (_, e)), _)| e < span.start)
        .take_while(|&((_, (b, e)), _)| b <= span.start && e >= span.start)
        .map(|((i, boundaries), line)| (i, boundaries, line));

    let mut i;
    for (line_number, (start, end), line) in lines_to_show {
        print_prefix(Some(line_number));
        eprintln!("{}", line);

        print_prefix(None);

        i = start;
        while i < span.start {
            eprint!(" ");
            i += 1;
        }

        if i == span.start {
            eprint!("^");
            i += 1;
        }

        while i < end && i < span.end {
            eprint!("~");
            i += 1;
        }

        if i == span.end && span.start != span.end {
            eprint!("^");
        }
        eprintln!();
    }
}

fn get_line_boundaries<'b>(source: &'b str) -> impl Iterator<Item = (usize, usize)> {
    source
        .chars()
        .enumerate()
        .filter_map(|(i, c)| if c == '\n' { Some(i) } else { None })
        .scan(0, |i: &mut usize, l: usize| {
            let res = if l != 0 { (*i, l - 1) } else { (0, 0) };
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

        let (number, (b, _)) = if span.start >= input.contents.len() {
            line_boundaries.last().unwrap()
        } else {
            line_boundaries
                .find(|&(_, (lb, le))| lb <= span.end && le >= span.start)
                .expect("valid span")
        };
        (number + 1, span.start - b + 1)
    };

    print_with_location(line_number, column_number, &msg);
    print_overlapping_lines(&input.contents, span);
}

pub fn print_error(input: &Input, diagnostic: &Diagnostic) {
    eprintln!("{}:", input.name);
    print_msg(
        &input,
        diagnostic.span,
        &format!("error: {}", diagnostic.msg),
    );

    for note in &diagnostic.notes {
        print_msg(&input, note.span, &format!("note: {}", note.value));
    }
}
