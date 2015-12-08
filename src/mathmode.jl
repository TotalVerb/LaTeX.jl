
COMMA = ","
BUILTIN_FN = Dict(
    :sin => "sin",
    :cos => "cos",
    :tan => "tan",
    :cot => "cot",
    :sec => "sec",
    :csc => "csc",
    :arcsin => "arcsin",
    :arccos => "arccos",
    :arctan => "arctan",
    :arccot => "arccot",
    :arcsec => "arcsec",
    :arccsc => "arccsc",
    :ln => "ln",
    :log => "log")

# math wrappers
abstract MathWrapper

getrequirements(::MathWrapper) = Dict("amsmath" => Set([]))

type AMSEquation <: MathWrapper
    math
end

type InlineMath <: MathWrapper
    math
end

function processitem(p, mw::AMSEquation, indent)
    r = Any["\\begin{equation}"]
    push!(r, processitem(p, mw.math, indent))
    push!(r, "\\end{equation}")
end

function processitem(p, mw::InlineMath, indent)
    r = Any["\\("]
    push!(r, processitem(p, mw.math, indent))
    push!(r, "\\)")
end

# math mode LaTeX
type PrefixOperator
    head
    args
    subscript
    superscript
end

type InfixOperator
    op
    args
end

type Fraction
    num
    den
end

type Root
    nth
    of
end

type Exponent
    base
    expt
end

function processitem(p, fc::PrefixOperator, indent)
    r = []
    if haskey(BUILTIN_FN, fc.head)
        push!(r, "\\$(BUILTIN_FN[fc.head])")
    else
        push!(r, "\\operatorname{$(fc.head)}")
    end
    if fc.subscript ≠ nothing
        push!(r, "_{")
        push!(r, processitem(p, fc.subscript, indent))
        push!(r, "}")
    end
    if fc.superscript ≠ nothing
        push!(r, "^{")
        push!(r, processitem(p, fc.superscript, indent))
        push!(r, "}")
    end
    push!(r, "{\\left(")
    for arg in fc.args
        push!(r, processitem(p, arg, indent))
        push!(r, COMMA)
    end
    pop!(r)  # remove last comma
    push!(r, "\\right)}")
end

function processitem(p, inf::InfixOperator, indent)
    r = Any[processitem(p, inf.args[1], indent)]
    for arg in inf.args[2:end]
        push!(r, string(inf.op))
        push!(r, processitem(p, arg, indent))
    end
    r
end

function processitem(p, frac::Fraction, indent)
    r = Any["\\frac{"]
    push!(r, processitem(p, frac.num, indent))
    push!(r, "}{")
    push!(r, processitem(p, frac.den, indent))
    push!(r, "}")
end

function processitem(p, rt::Root, indent)
    if rt.nth == 2
        r = Any["\\sqrt{"]
    else
        r = Any["\\sqrt[$(rt.nth)]{"]
    end
    push!(r, processitem(p, rt.of, indent))
    push!(r, "}")
end

function processitem(p, ex::Exponent, indent)
    r = Any["{\\left("]
    push!(r, processitem(p, ex.base, indent))
    push!(r, "\\right)}^{")
    push!(r, processitem(p, ex.expt, indent))
    push!(r, "}")
end

processitem(p, i::Integer, indent) = string(i)

function texify_call(fn, args)
    if fn ∈ (:+, :-)  # infix or prefix unary/binary ops
        InfixOperator(fn, map(texify_math, args))
    elseif fn == :*
        InfixOperator("\\cdot", map(texify_math, args))
    elseif fn ∈ (:(==), :≡, :is, :(===), :isequal)
        InfixOperator("=", map(texify_math, args))
    elseif fn ∈ (:(!=), :≠)
        InfixOperator("=", map(texify_math, args))
    elseif fn ∈ (:/, ://)
        Fraction(map(texify_math, args)...)
    elseif fn == :log10
        PrefixOperator(:log, map(texify_math, args), 10, nothing)
    elseif fn == :log2
        PrefixOperator(:log, map(texify_math, args), 2, nothing)
    elseif fn ∈ (:sqrt, :√)
        Root(2, texify_math(args[1]))
    elseif fn == :^
        Exponent(texify_math(args[1]), texify_math(args[2]))
    elseif fn ∈ (:sinpi, :cospi)
        PrefixOperator(
            split(string(fn), "pi")[1],
            [texify_call(:*, [:π, args[1]])],
            nothing, nothing)
    else
        PrefixOperator(fn, map(texify_math, args), nothing, nothing)
    end
end

"""
Texify a Julia expression into a LaTeX math-mode structure.
"""
function texify_math(ex::Expr)
    if ex.head == :call
        texify_call(ex.args[1], ex.args[2:end])
    elseif ex.head == :block
        filter(x -> x ≠ nothing, map(texify_math, ex.args))
    elseif ex.head == :(=)
        InfixOperator("=", map(texify_math, ex.args))
    end
end

texify_math(ex::LineNumberNode) = nothing
texify_math(ex::Integer) = ex
texify_math(ex::Symbol) =
    ( ex == :im        ? "i"
    : ex ∈ [:π, :pi]   ? "\\pi"
    : string(ex))

"""
Texify a Julia expression into a LaTeX math-mode structure, then wrap it in the
specified kind of wrapper (one of `:equation`, `:inline`, and `:none`).
"""
function texify(ex::Expr, as::Symbol=:equation)
    if as == :equation
        AMSEquation(texify_math(ex))
    elseif as == :inline
        InlineMath(texify_math(ex))
    else
        texify_math(ex)
    end
end


Base.writemime(io::IO, ::MIME"text/latex", obj::MathWrapper) =
    print(io, join(flatten(processitem(Dict(), obj, 0)), " "))
