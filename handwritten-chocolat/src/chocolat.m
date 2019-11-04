#import <Foundation/Foundation.h>

#if !__has_feature(objc_arc)
#error This file must be compiled with ARC turned on (-fobjc-arc)
#endif

static void abort_on_exception(__unsafe_unretained NSException *exception) __attribute__((noreturn));

static void abort_on_exception(__unsafe_unretained NSException *exception) {
    NSLog(@"Unexpected exception: %@", exception.reason);
    abort();
}

NSUInteger chocolat_Foundation_NSObjectProtocol_instance_hash(id<NSObject> self_) {
    @try {
        return self_.hash;
    }
    @catch (NSException *exception) {
        abort_on_exception(exception);
    }
}

__attribute__((ns_returns_retained)) NSObject *chocolat_Foundation_NSObjectInterface_class_new(void) {
    @try {
        return [NSObject new];
    }
    @catch (NSException *exception) {
        abort_on_exception(exception);
    }
}

signed char chocolat_Foundation_NSObjectProtocol_instance_isEqual(__unsafe_unretained id<NSObject> self_, __unsafe_unretained id object) {
    @try {
        return [self_ isEqual:object];
    }
    @catch (NSException *exception) {
        abort_on_exception(exception);
    }
}

__attribute__((ns_returns_retained)) NSString *chocolat_Foundation_NSObjectProtocol_instance_description(__unsafe_unretained id<NSObject> self_) {
    @try {
        return self_.description;
    }
    @catch (NSException *exception) {
        abort_on_exception(exception);
    }
}

NSUInteger chocolat_Foundation_NSStringInterface_instance_lengthOfBytesUsingEncoding(__unsafe_unretained NSString *self_, NSStringEncoding encoding) {
    @try {
        return [self_ lengthOfBytesUsingEncoding:encoding];
    }
    @catch (NSException *exception) {
        abort_on_exception(exception);
    }
}

const char *chocolat_Foundation_NSStringInterface_instance_UTF8String(__unsafe_unretained NSString *self_) {
    @try {
        return [self_ UTF8String];
    }
    @catch (NSException *exception) {
        abort_on_exception(exception);
    }
}

__attribute__((ns_returns_retained)) NSString *chocolat_Foundation_NSStringInterface_class_newWithBytes_length_encoding(const void *bytes, NSUInteger len, NSStringEncoding encoding) {
    @try {
        return [[NSString alloc] initWithBytes:bytes length:len encoding:encoding];
    }
    @catch (NSException *exception) {
        abort_on_exception(exception);
    }
}

@interface Bar: NSObject
@property (nonatomic) Bar *bar;
@end

@implementation Bar
- (void)dealloc
{
    NSLog(@"Bar dealloc");
}
@end

@interface Foo: NSObject
@property (nonatomic) Bar *bar;
@end

@implementation Foo

- (instancetype)init
{
    self = [super init];
    _bar = [Bar new];
    return self;
}

- (void)dealloc
{
    NSLog(@"Foo dealloc");
}

@end

__attribute__((ns_returns_retained)) Foo *chocolat_Foundation_FooInterface_class_new(void) {
    @try {
        return [Foo new];
    }
    @catch (NSException *exception) {
        abort_on_exception(exception);
    }
}

__attribute__((ns_returns_retained)) Bar *chocolat_Foundation_FooInterface_instance_bar(Foo *self_) {
    @try {
        return self_.bar;
    }
    @catch (NSException *exception) {
        abort_on_exception(exception);
    }
}
