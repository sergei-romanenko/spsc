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

function Base.show(io::IO, call::Call)
    print(io, call.name)
    l = length(call.args) 
    if call.ckind isa Ctr && l == 0
        return
    end
    showPList(io, call.args)
end

function Base.show(io::IO, binding::Binding)
    print(io, binding.name)
    print(io, "=")
    print(io, binding.exp)
end

function Base.show(io::IO, e::Let)
    print(io, "let ")
    showList(io, e.bindings)
    print(io, " in ")
    print(io, e.exp)
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
    if length(g.cparams) > 0
        showPList(io, g.cparams)
    end
    if length(g.params) > 0
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

