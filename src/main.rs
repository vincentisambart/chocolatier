#![allow(dead_code)]
#![allow(clippy::cognitive_complexity)]
#![warn(rust_2018_idioms)]

mod ast;
mod clang;
mod generator;

fn main() {
    let source = r#"
        // @interface I
        // - (id)foo __attribute__((__ns_returns_retained__));
        // @end
        // __attribute__((ns_returns_retained)) id foo(void);

        #import <Foundation/Foundation.h>
        #import <Cocoa/Cocoa.h>
        #import <AVFoundation/AVFoundation.h>
        #import <AVKit/AVKit.h>
        #import <Metal/Metal.h>
        #import <MetalKit/MetalKit.h>

        // @interface I
        // @end

// typedef const struct __attribute__((objc_bridge(NSString))) __CFString * CFStringRef;

        // typedef enum { A = 1 } foo;
        // enum E { B = 1000 };
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
    // ast::print_full_clang_ast(source);
    let decls = ast::ast_from_str(source).unwrap();
    // println!("{:#?}", decls);
    let mut generator = generator::Generator::new(decls);
    generator.generate();
}
