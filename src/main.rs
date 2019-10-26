#![allow(dead_code)]
#![allow(clippy::cognitive_complexity)]

mod ast;

fn main() {
    let source = r#"
        // @interface I
        // - (id)foo __attribute__((__ns_returns_retained__));
        // @end
        // __attribute__((ns_returns_retained)) id foo(void);

        // #import <AVFoundation/AVFoundation.h>
        // #import <Cocoa/Cocoa.h>

// typedef const struct __attribute__((objc_bridge(NSString))) __CFString * CFStringRef;

        typedef enum { A = 1 } foo;
        enum E { B = 1000 };
        // typedef signed long CFIndex;
        // typedef enum __attribute__((enum_extensibility(open))) CFStreamStatus : CFIndex CFStreamStatus; enum CFStreamStatus : CFIndex {
        //     kCFStreamStatusNotOpen = 0,
        //     kCFStreamStatusOpening,
        //     kCFStreamStatusOpen,
        //     kCFStreamStatusReading,
        //     kCFStreamStatusWriting,
        //     kCFStreamStatusAtEnd,
        //     kCFStreamStatusClosed,
        //     kCFStreamStatusError
        // };
        // struct ll { struct ll *nextl; };
        // struct { float f; union { int i; double d; }; } a;
  "#;
    let decls = ast::ast_from_str(source).unwrap();
    ast::print_full_clang_ast(source);
    println!("{:#?}", decls);
}
