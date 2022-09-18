use super::token;
use crate::error::ErrorKind;

impl TryFrom<token::Number<'_>> for i32 {
    type Error = ErrorKind;

    fn try_from(token::Number { raw, .. }: token::Number) -> Result<Self, ErrorKind> {
        let value = raw.parse::<f32>().map_err(|_| ErrorKind::InvalidNumber)?;
        if value.fract() == 0.0 {
            // SAFETY: f32 parsed from source text will never be NaN or infinity.
            unsafe { Ok(value.to_int_unchecked()) }
        } else {
            Err(ErrorKind::ExpectInteger)
        }
    }
}
