module ShowUtil

using SPSC.SLanguage

function showList(io::IO, list::Vector{T}) where {T}
    for i in 1:length(list)
        if i != 1
            print(io, ",")
        end
        print(io, list[i])
    end
end

function showPList(io::IO, list::Vector{T}) where {T}
    print(io, "(")
    showList(io, list)
    print(io, ")")
end

function Base.show(io::IO, v::Var)
    print(io, v.name)
end

function Base.show(io::IO, call::CFG)
    print(io, call.name)
    if !(call.kind == Ctr) || !isempty(call.args)
        showPList(io, call.args)
    end
end

function Base.show(io::IO, binding::Binding)
    print(io, binding.name)
    print(io, "=")
    print(io, binding.e)
end

function Base.show(io::IO, e::Let)
    print(io, "let ")
    if !isempty(e.bindings)
        showList(io, e.bindings)
        print(io, " ")
    end
    print(io, "in ")
    print(io, e.body)
end

function Base.show(io::IO, f::FRule)
    print(io, f.name)
    showPList(io, f.params)
    print(io, "=")
    print(io, f.body)
    print(io, ";")
end

function Base.show(io::IO, g::GRule)
    print(io, g.name)
    print(io, "(")
    print(io, g.cname)
    if !isempty(g.cparams)
        showPList(io, g.cparams)
    end
    if !isempty(g.params)
        print(io, ",")
        showList(io, g.params)
    end
    print(io, ")=")
    print(io, g.body)
    print(io, ";")
end

function Base.show(io::IO, prog::Program)
    for rule in prog.rules
        print(io, rule)
    end
end

end

