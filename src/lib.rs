mod bytecode;
mod compiler;
mod vm;

pub use compiler::Compiler;
pub use vm::{Value, Vm};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scoping() {
        assert_result(
            r#"
                let x = 2;
                let y = 3;
                let z = 5;
                {
                    let y = 7;
                    let z = 13;
                    {
                        let z = 17;
                        x * y * z
                    }
                }
        "#,
            Value::Number(2. * 7. * 17.),
        );
    }

    #[test]
    fn rebinding() {
        assert_result(
            r#"
                let x = 2;
                let y = x;
                let x = 3;
                x * y
        "#,
            Value::Number(3. * 2.),
        );
    }

    fn assert_result(code: &str, value: Value) {
        assert_eq!(
            Vm::default()
                .execute(&compiler::Compiler::default().compile(code).unwrap())
                .unwrap(),
            value
        )
    }
}
