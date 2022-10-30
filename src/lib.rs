#![deny(unsafe_code)]
#![deny(
    clippy::all,
    clippy::pedantic,
    clippy::nursery,
    clippy::unwrap_used,
    clippy::expect_used
)]
#![allow(clippy::missing_errors_doc)]

mod bytecode;
mod compiler;
mod vm;

pub use compiler::Compiler;
pub use vm::{Value, Vm};

pub static PRELUDE: &str = r#"
let nd = |n, d|
    if n == 0 
        then 0 
        else __random d + nd(n - 1, d);

let min = |a, b| {
    let av = a + 0;
    let bv = b + 0;
    if a < b then a else b
};
let max = |a, b| {
    let av = a + 0;
    let bv = b + 0;
    if a > b then a else b
};

let nd4 = |n| nd(n,4);
let d4 = || nd4(1);

let nd6 = |n| nd(n,6);
let d6 = || nd6(1);

let nd8 = |n| nd(n,8);
let d8 = || nd8(1);

let nd10 = |n| nd(n,10);
let d10 = || nd10(1);

let nd12 = |n| nd(n,12);
let d12 = || nd12(1);

let nd20 = |n| nd(n,20);
let d20 = || nd20(1);

let nd100 = |n| nd(n,100);
let d100 = || nd100(1);
"#;

#[cfg(test)]
#[allow(clippy::unwrap_used)]
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
            let sub = |a, b| a - b;
            let div = |a, b| a / b;
            div(7, sub(5, 3))
        "#,
            Value::Number(3.5),
        );
    }

    #[test]
    fn calls_as_numbers() {
        assert_result(r#"d12 + d6"#, Value::Number(12.));
    }

    #[test]
    fn if_condition() {
        assert_result(r#"if 5 > 3 then 10 else 20"#, Value::Number(10.));
    }

    #[test]
    fn else_condition() {
        assert_result(r#"if 2 > 3 then 10 else 20"#, Value::Number(20.));
    }

    #[test]
    fn nested_if_else() {
        assert_result(
            r#"if 2 > 3 then if 5 < 3 then 10 else 20 else if 5 > 3 then 30 else 40"#,
            Value::Number(30.),
        );
    }

    #[test]
    fn nd() {
        assert_result(r#"nd12(5,)"#, Value::Number(25.));
    }

    #[test]
    fn final_value_eval_no_arg_func() {
        assert_result(r#"d20"#, Value::Number(5.));
    }

    #[test]
    fn min_max() {
        assert_result(r#"min(max(17, 13), 23)"#, Value::Number(17.));
        assert_result(r#"min(d20, d20)"#, Value::Number(18.));
    }

    fn assert_result(code: &str, value: Value) {
        init();
        let mut tmp = String::from(PRELUDE);
        tmp.push_str(code);
        let chunk = compiler::Compiler::default().compile(tmp.as_str()).unwrap();
        let mut vm = Vm::default();
        vm.rng.seed(12345);
        assert_eq!(vm.execute(&chunk).unwrap(), value);
    }
}
