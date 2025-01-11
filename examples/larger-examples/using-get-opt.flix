use GetOpt.{ArgDescr, OptionDescr}
use GetOpt.ArgDescr.{NoArg, ReqArg, OptArg}
use GetOpt.ArgOrder.{RequireOrder, Permute, ReturnInOrder}

use GetOpt.{getOpt, usageInfo}

/// Define a datatype representing all flags and options.
enum Flag with ToString {
    case Version
    case Help
    case Verbose
    case Threads(Int32)
    case Listen(Int32)
}

/// Define a list of containing an option descriptors for each flag or option.
def options(): List[OptionDescr[Flag]] =
       {optionIds = 'v' :: Nil,         optionNames = "version" :: Nil,     argDescriptor = NoArg(Flag.Version),            explanation = "prints the version number"}
    :: {optionIds = 'h' :: '?' :: Nil,  optionNames = "help" :: Nil,        argDescriptor = NoArg(Flag.Help),               explanation = "prints this usage information"}
    :: {optionIds = 'X' :: Nil,         optionNames = "verbose" :: Nil,     argDescriptor = NoArg(Flag.Verbose),            explanation = "enables verbose output"}
    :: {optionIds = 'o':: Nil,          optionNames = "threads" :: Nil,     argDescriptor = ReqArg(threads, "<threads>"),   explanation = "number of <threads> to use"}
    :: {optionIds = 'i':: Nil,          optionNames = "listen" :: Nil,      argDescriptor = ReqArg(listen,  "<port>"),      explanation = "starts the socket server and listens on <port>" }
    :: Nil

/// Define a simple "parser" to read an integer number of threads.
/// (Used by the "threads" option descriptor)
def threads(x: String): Option[Flag] = Int32.fromString(x) |> Option.map(i -> Flag.Threads(i))

/// Define a simple "parser" to read an integer port number.
/// (Used by the "listen" option descriptor)
def listen(x: String): Option[Flag] = Int32.fromString(x) |> Option.map(i -> Flag.Listen(i))

/// Decode the array `args` supplied to main.
/// Note in this case we want to allow file names to contain spaces - the initial array `args` has
/// been split on the Space character, we use `GetOpt.preprocess` to rejoin file names that were enclosed in
/// single-quotes. The new array of arguments with file names rejoined is `args2`.
def main(): Unit \ {Env, Sys, IO} =
    let args = Environment.handle(Environment.getArgs)();
    let args2 = GetOpt.preprocess({quoteOpen = "'", quoteClose = "'", stripQuoteMarks = false}, args);
    let header = "Usage: <program> [OPTION...] files...";
    let options1 = options();
    match getOpt(Permute, options1, args2) {
        case Validation.Success(opts)   => {println("Optionals: ${opts#options}"); println("Non-optionals: ${opts#nonOptions}")}
        case Validation.Failure(errs)   => {println("Errors: ${errs}");  println(usageInfo(header, options1))}
    }
