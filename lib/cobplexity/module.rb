module Cobplexity

  class Module
    attr_reader :code, :lines, :paragraphs
    def code= code
      @code = code
      analyze_code
    end
    def analyze_code
      reset_data
      @code.lines.each do |line|
        @line = Line.new line
        if @line.procedure_division?
          reset_data
        else
          count_module
          count_paragraph
        end
      end
    end
    def reset_data
      @lines = 0
      @paragraphs = []
    end
    def count_module
      @lines += 1 if @line.code? 
    end
    def count_paragraph
      @paragraphs << Paragraph.new(@line.paragraph_name) if @line.paragraph?
      if @line.code? && !@paragraphs.last.nil?
        @paragraphs.last.lines += 1
        @paragraphs.last.complexity += @line.branches
      end
    end
  end

end
