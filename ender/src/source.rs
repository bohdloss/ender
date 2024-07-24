use core::fmt::Formatter;
use parse_display::Display;

#[derive(Copy, Clone, Debug, Display)]
#[cfg_attr(not(feature = "debug"), allow(dead_code))]
pub enum Frame {
    #[display("{0}")]
    Item(&'static str),
    #[display("{0}")]
    Field(&'static str),
    #[display("{0}")]
    Variant(&'static str),
    #[display("{0}")]
    Index(usize),
}

#[derive(Clone, Debug)]
pub struct Stack {
    #[cfg(feature = "alloc")]
    pub(crate) frames: alloc::vec::Vec<Frame>,
    #[cfg(not(feature = "alloc"))]
    pub(crate) last_frame: Frame,
}

impl core::fmt::Display for Stack {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        #[cfg(feature = "alloc")]
        {
            let mut needs_separator = false;
            for frame in self.frames.iter() {
                match frame {
                    Frame::Item(x) => {
                        if needs_separator {
                            write!(f, " ... {x}")?;
                        } else {
                            write!(f, "{x}")?;
                        }
                        needs_separator = true;
                    }
                    Frame::Field(x) => {
                        write!(f, " -> {x}")?;
                    }
                    Frame::Variant(x) => {
                        write!(f, "::{x}")?;
                    }
                    Frame::Index(x) => {
                        write!(f, "[{x}]")?;
                    }
                }
            }
        }
        #[cfg(not(feature = "alloc"))]
        {
            let ref last_frame = self.last_frame;
            write!(f, "{last_frame}")?;
        }

        Ok(())
    }
}

impl Stack {
    #[inline]
    pub const fn new() -> Self {
        Self {
            #[cfg(feature = "alloc")]
            frames: alloc::vec::Vec::new(),
            #[cfg(not(feature = "alloc"))]
            last_frame: Frame::Item(""),
        }
    }
}
