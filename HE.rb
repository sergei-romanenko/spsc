require 'SLL'
require 'Algebra'

module HE

  extend SLL
  # Enhanced homeomorphic embedding:
  # expressions are compared only if they belong
  # to the same category (as defined by `aVarIsUnderAttack`).
  def embeddedIn(e1, e2)
    aVarIsUnderAttack(e1) == aVarIsUnderAttack(e2) && he(e1, e2)
  end

  # This is the "classic" homeomorphic embedding relation.

  def he(e1, e2)
    heByDiving(e1, e2) || heByCoupling(e1, e2)
  end

  def heByDiving(e1, e2)
    if e2.isVar()
      false
    elsif e2.isCall()
      e2.args.find{|e2arg| he(e1, e2arg)} != nil
    end
  end

  def heByCoupling(e1, e2)
    if e1.isVar() && e2.isVar()
      true
    elsif e1.hasTheSameFunctorAs?(e2)
      e1.args.zip(e2.args).each do |e1arg, e2arg|
        if ! he(e1arg, e2arg)
          return false
        end
      end
      true
    end
  end

  # We distinguish a specific category of expressions:
  # the ones that generate contractions in the process tree.

  def aVarIsUnderAttack(e)
    if e.isGCall()
      aVarIsUnderAttack(e.args[0])
    else
      e.isVar()
    end
  end

end