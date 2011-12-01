module Cobplexity

  class Paragraph
    attr_reader :name
    attr_accessor :lines, :complexity
    def initialize name
      @name = name
      @lines = 0
      @complexity = 1
    end
  end

end
