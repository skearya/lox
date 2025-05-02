use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};

use crate::token::{Token, TokenKind};

pub fn unexpected(expected: &TokenKind, unexpected: &Token, src: &str, note: &str) {
    let span = unexpected.span(src);

    let a = Color::BrightMagenta;
    let b = Color::BrightGreen;

    Report::build(ReportKind::Error, ("input.lox", span))
        .with_message(format!(
            "expected `{expected}` found `{}`",
            unexpected.lexeme
        ))
        .with_label(
            Label::new(("input.lox", unexpected.span(src)))
                .with_message(format!(
                    "Found {} looking for {}",
                    unexpected.lexeme.fg(a),
                    expected.fg(b)
                ))
                .with_color(a),
        )
        .with_note(note)
        .finish()
        .eprint(("input.lox", Source::from(src)))
        .unwrap();
}
