use codespan::ByteIndex;
use lalrpop_util::ParseError;
use logos::{Logos, SpannedIter};
use void::Void;

use crate::tokens::Token; // your enum

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

pub struct Lexer<'input> {
    // instead of an iterator over characters, we have a token iterator
    token_stream: SpannedIter<'input, Token<'input>>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        // the Token::lexer() method is provided by the Logos trait
        Self {
            token_stream: Token::lexer(input).spanned(),
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token<'input>, ByteIndex, ParseError<ByteIndex, Token<'input>, Void>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.token_stream.next().map(|(token, span)| match token {
            Ok(token) => Ok((
                ByteIndex::from(span.start as u32),
                token,
                ByteIndex::from(span.end as u32),
            )),
            Err(_) => Err(ParseError::<ByteIndex, Token<'input>, Void>::InvalidToken {
                location: ByteIndex::from(span.start as u32),
            }),
        })
    }
}
