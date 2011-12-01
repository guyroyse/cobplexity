module Cobplexity

  class Analyzer
    attr_accessor :threshold, :group
    def initialize
      @threshold = 5
      @group = "APP"
    end
  end

end
