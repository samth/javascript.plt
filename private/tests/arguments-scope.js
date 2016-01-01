var observations = []

function fact(n) {
    observations[observations.length] = arguments[0]
    if (n <= 1) { return 1 }
    var rest = fact(n - 1)
    observations[observations.length] = arguments[0]
    return n * rest
}

var result = [ fact(5), observations ]
