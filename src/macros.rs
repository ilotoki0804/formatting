#[macro_export]
macro_rules! literal_map {
    ({ $($key:expr => $value:expr),* $(,)? }) => {
        {
            let mut map = std::collections::HashMap::new();
            $(
                map.insert($key.to_string(), $value);
            )*
            map
        }
    };

    ( $map_type:ty, { $($key:expr => $value:expr),* $(,)? }) => {
        {
            let mut map: $map_type = std::collections::HashMap::new();
            $(
                map.insert($key.to_string(), $value);
            )*
            map
        }
    };

    ($map:expr, { $($key:expr => $value:expr),* $(,)? }) => {
        {
            let mut initialized = $map;
            $(
                initialized.insert($key.to_string(), $value);
            )*
            initialized
        }
    };
}

#[macro_export]
macro_rules! format_template {
    ($template:literal, $($key:ident = $value:expr),* $(,)? ) => {
        {
            use $crate::Template;
            let calculated_template: Template = $template.parse().unwrap();
            
            format_template!(calculated_template, $(
                $key=$value,
            )*)
        }
    };

    ($template:expr, $($key:ident = $value:expr),* $(,)? ) => {
        {
            use $crate::{Getter, Formattable};
            let result: anyhow::Result<String> = (||{
                struct RuntimeGetter<'a> {
                    $(
                        $key: &'a dyn Formattable,
                    )*
                }
    
                impl<'a> Getter for RuntimeGetter<'a> {
                    fn get_formattable(&self, key: &str) -> Option<&dyn Formattable> {
                        match key {
                            $(
                                stringify!($key) => Some(self.$key),
                            )*
                            _ => None
                        }
                    }
                }
    
                let runtime_getter = RuntimeGetter {
                    $(
                        $key: $value,
                    )*
                };
    
                let result = $template.format(&runtime_getter)?;
    
                Ok(result)
            })();
            result
        }
    };

    ($template:literal, $($value:expr),* $(,)? ) => {
        {
            use $crate::Template;
            let calculated_template: Template = $template.parse().unwrap();
            
            format_template!(calculated_template, $(
                $value,
            )*)
        }
    };

    ($template:expr, $($value:expr),* $(,)? ) => {
        {
            use $crate::{Getter, Formattable};
            let result: anyhow::Result<String> = (||{
                const ARG_COUNT: usize = format_template!(@count_args, $( $value ),* );
                struct RuntimeGetter<'a> {
                    args: [&'a dyn Formattable; ARG_COUNT],
                }
            
                impl<'a> Getter for RuntimeGetter<'a> {
                    fn get_formattable(&self, key: &str) -> Option<&dyn Formattable> {
                        let key: usize = key.parse().ok()?;
                        if key >= ARG_COUNT {
                            return None;
                        }
                        Some(self.args[key])
                    }
                }
            
                let runtime_getter = RuntimeGetter {
                    args: [ $( $value ),* ],
                };
            
                let result = $template.format(&runtime_getter)?;

                Ok(result)
            })();
            result
        }
    };

    // arg 개수 세기
    // https://users.rust-lang.org/t/enumerate-macro-repeating-elements/96318
    (@count_args $(,)?) => {
        0
    };
    (@count_args, $_:expr) => {
        1
    };
    (@count_args, $_:expr, $($tail:expr),* $(,)? ) => {
        1 + format_template!(@count_args, $($tail),* )
    }
}

#[cfg(test)]
mod test {
    use crate::{Formattable, Getter, Template};

    #[test]
    fn test_without_macro() {
        let result: anyhow::Result<String> = (||{
            let hello = "hmm".to_string();
            let num = 123;
        
            struct RuntimeGetter<'a> {
                hello: &'a dyn Formattable,
                num: &'a dyn Formattable,
            }
        
            impl<'a> Getter for RuntimeGetter<'a> {
                fn get_formattable(&self, key: &str) -> Option<&dyn Formattable> {
                    match key {
                        stringify!(hello) => Some(self.hello),
                        stringify!(num) => Some(self.num),
                        _ => None
                    }
                }
            }
        
            let runtime_getter = RuntimeGetter {
                hello: &hello,
                num: &num,
            };
        
            let template: Template = "hello {hello} num {num:04}".parse()?;
            let result = template.format(&runtime_getter)?;
            
            Ok(result)
        })();
        assert_eq!(result.unwrap(), "hello hmm num 0123");
    }
    

    #[test]
    fn test_without_macro_pos() {
        let result: anyhow::Result<String> = (||{
            let one = "hmm".to_string();
            let two = 123;
        
            struct RuntimeGetter<'a> {
                args: [&'a dyn Formattable; 2],
            }
        
            impl<'a> Getter for RuntimeGetter<'a> {
                fn get_formattable(&self, key: &str) -> Option<&dyn Formattable> {
                    let key: usize = key.parse().ok()?;
                    if key >= 2 {
                        return None;
                    }
                    Some(self.args[key])
                }
            }
        
            let runtime_getter = RuntimeGetter {
                args: [&one, &two],
            };
        
            let template: Template = "hello {} num {:04}".parse()?;
            let result = template.format(&runtime_getter)?;

            Ok(result)
        })();
        assert_eq!(result.unwrap(), "hello hmm num 0123");
    }

    #[test]
    fn test_format_macro() {
        let hello = "hmm".to_string();
        let num = 123;

        let template: Template = "hello {hello} num {num:04}".parse().unwrap();
        let result = format_template!(template, hello=&hello, num=&num).unwrap();
        assert_eq!(result, "hello hmm num 0123");
    }
    
    
    #[test]
    fn test_format_macro_pos() {
        let hello = "hmm".to_string();
        let num = 123;

        let template: Template = "hello {} num {:04}".parse().unwrap();
        let result = format_template!(template, &hello, &num).unwrap();
        assert_eq!(result, "hello hmm num 0123");
    }
    
    #[test]
    fn test_literal_format_macro() {
        let hello = "hmm".to_string();
        let num = 123;

        let result = format_template!("hello {hello} num {num:04}", hello=&hello, num=&num).unwrap();
        assert_eq!(result, "hello hmm num 0123");
    }
    
    #[test]
    fn test_literal_format_macro_pos() {
        let hello = "hmm".to_string();
        let num = 123;

        let result = format_template!("hello {} num {:04}", &hello, &num).unwrap();
        assert_eq!(result, "hello hmm num 0123");
    }
}
