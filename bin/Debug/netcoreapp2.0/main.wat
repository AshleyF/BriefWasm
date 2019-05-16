(module
  (type (;0;) (func (param i32) (result i32)))
  (func (;0;) (type 0) (param i32) (result i32)
    (local i32 i32)
    global.get 0
    drop
    local.get 0
    i32.const 123
    i32.add
    local.set 1
    i32.const 3
    i32.const 4
    i32.add
    i32.const 5
    i32.mul
    local.get 1
    i32.add)
  (global (;0;) i32 (i32.const 123))
  (export "main" (func 0)))
