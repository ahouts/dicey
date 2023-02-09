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
let min = |a, b| {
    let av = a + 0;
    let bv = b + 0;
    if av < bv then av else bv
};
let max = |a, b| {
    let av = a + 0;
    let bv = b + 0;
    if av > bv then av else bv
};
"#;

#[cfg(test)]
#[allow(clippy::unwrap_used)]
mod tests {
    use std::sync::{Arc, Mutex};

    use super::*;
    use anyhow::Result;
    use once_cell::sync::OnceCell;

    static LOG: OnceCell<()> = OnceCell::new();

    fn init() {
        LOG.get_or_init(|| simple_logger::init_with_level(log::Level::Trace).unwrap());
    }

    #[test]
    fn scoping() {
        assert_value(
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
            &Value::Number(2. * 7. * 17.),
        );
    }

    #[test]
    fn rebinding() {
        assert_value(
            r#"
                let x = 2;
                let y = x;
                let x = 3;
                x * y
        "#,
            &Value::Number(3. * 2.),
        );
    }

    #[test]
    fn calls() {
        assert_value(
            r#"
            let sub = |a, b| a - b;
            let div = |a, b| a / b;
            div(7, sub(5, 3))
        "#,
            &Value::Number(3.5),
        );
    }

    #[test]
    fn calls_as_numbers() {
        assert_value(r#"(|| 1) + (|| 2)"#, &Value::Number(3.));
    }

    #[test]
    fn if_condition() {
        assert_value(r#"if 5 > 3 then 10 else 20"#, &Value::Number(10.));
    }

    #[test]
    fn else_condition() {
        assert_value(r#"if 2 > 3 then 10 else 20"#, &Value::Number(20.));
    }

    #[test]
    fn nested_if_else() {
        assert_value(
            r#"if 2 > 3 then if 5 < 3 then 10 else 20 else if 5 > 3 then 30 else 40"#,
            &Value::Number(30.),
        );
    }

    #[test]
    fn nd() {
        assert_value(r#"5d12"#, &Value::Number(19.));
    }

    #[test]
    fn final_value_eval_no_arg_func() {
        assert_value(r#"|| d20"#, &Value::Number(9.));
    }

    #[test]
    fn min_max() {
        assert_value(r#"min(max(17, 13), 23)"#, &Value::Number(17.));
    }

    #[test]
    fn min_dice() {
        assert_value(r#"min(d20, d20)"#, &Value::Number(6.));
    }

    #[test]
    fn mod_works() {
        assert_value(r#"10 % 3"#, &Value::Number(1.));
        assert_value(r#"10.5 % 1"#, &Value::Number(0.5));
    }

    #[test]
    fn chained_final_function_value() {
        assert_value(r#"let a = || 3; let b = || a; b"#, &Value::Number(3.));
    }

    #[test]
    fn infinite_recursion() {
        assert_err(r#"let a = || a; a"#, "stack overflow");
    }

    #[test]
    fn infinite_recursion2() {
        assert_err(r#"let a = || a + a; a"#, "stack overflow");
    }

    #[test]
    fn ambiguous_identifier() {
        assert_err(
            r#"let d10 = 1; d10"#,
            "identifier d10 is ambiguous with a dice roll",
        );
    }

    #[test]
    fn ambiguous_parameter() {
        assert_err(r#"|d5| 5"#, "identifier d5 is ambiguous with a dice roll");
    }

    #[test]
    fn d_acceptable_local() {
        assert_value(r#"let d = 5; d"#, &Value::Number(5.));
    }

    #[test]
    fn list() {
        assert_value(
            r#"let x = [1, false, 2, true]; [ x[3], x[1], x[2], x[0] ]"#,
            &Value::List(Arc::new(Mutex::new(vec![
                Value::Boolean(true),
                Value::Boolean(false),
                Value::Number(2.),
                Value::Number(1.),
            ]))),
        );
    }

    #[test]
    fn list_out_of_bounds() {
        assert_err(r#"[false][1]"#, "index 1 out of bounds");
        assert_err(r#"[false][-1]"#, "invalid index");
        assert_err(r#"[][0]"#, "index 0 out of bounds");
    }

    #[test]
    fn bad_index() {
        assert_err(r#"false[0]"#, "tried to peek list, found false");
        assert_err(r#"1[0]"#, "tried to peek list, found 1");
        assert_err(r#"(|| false)[0]"#, "tried to peek list, found <function>");
    }

    fn assert_value(code: &str, value: &Value) {
        assert_eq!(&get_result(code).unwrap(), value);
    }

    fn assert_err(code: &str, error_msg: &str) {
        assert_eq!(format!("{}", get_result(code).unwrap_err()), error_msg);
    }

    fn get_result(code: &str) -> Result<Value> {
        init();
        let mut tmp = String::from(PRELUDE);
        tmp.push_str(code);
        let chunk = compiler::Compiler::default().compile(tmp.as_str())?;
        let mut vm = Vm::default();
        vm.rng.seed(12345);
        vm.execute(&chunk)
    }
}
