use logos::Span;
use chic::Error;
use line_span::{find_line_start, find_prev_line_start};

#[macro_export]
macro_rules! response {
  ( $( $r:expr ),+ ) => {{
    $(
        print!("{}", $r);
    )*
    println!();
  }};
}

pub fn error(src: &String, error: &str, label: &str, help: &str, span: Span) {
    let line = get_line(src, span.clone());

    let start = span.start + line;
    let end   = span.end + line;// + prev;

    response!(
        self::Error::new(error)
            .error(line, start, end, src, label)
            .help(help)
            .to_string()
    )
}

fn get_line(src: &String, span: Span) -> usize {
    src[..span.start].chars().filter(|x| x == &'\n').count()
}