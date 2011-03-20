let (@@) f x = f x
let id x = x
let (|>) x f = f x
let const c _ = c
let ($) f g x = f (g x)
