mod bytecode;
mod compiler;
mod vm;

pub use compiler::Compiler;
pub use vm::{Value, Vm};

pub static PRELUDE: &str = r#"
let nd = fn (n,) __random n;
let d4 = fn () nd(4,);
let d6 = fn () nd(6,);
let d8 = fn () nd(8,);
let d10 = fn () nd(10,);
let d12 = fn () nd(12,);
let d20 = fn () nd(20,);
let d100 = fn () nd(100,);
"#;

#[cfg(test)]
mod tests {
    use once_cell::sync::OnceCell;

    use super::*;

    static LOG: OnceCell<()> = OnceCell::new();

    fn init() {
        LOG.get_or_init(|| simple_logger::init_with_level(log::Level::Warn).unwrap());
    }

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

    #[test]
    fn calls() {
        assert_result(
            r#"
            let sub = fn (a, b,) a - b;
            let div = fn (a, b,) a / b;
            div(7, sub(5, 3,),)
        "#,
            Value::Number(3.5),
        );
    }

    fn assert_result(code: &str, value: Value) {
        init();
        let mut tmp = String::from(PRELUDE);
        tmp.push_str(code);
        let chunk = compiler::Compiler::default().compile(tmp.as_str()).unwrap();
        let mut vm = Vm::default();
        vm.rng.seed(12345);
        assert_eq!(vm.execute(&chunk).unwrap(), value)
    }
}
