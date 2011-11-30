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
      @paragraphs.last.lines += 1 if @line.code? && !@paragraphs.last.nil?
    end
  end

  class Line
    def initialize line
      @line = line.strip
    end
    def code?
      !self.blank? && !self.comment? && !self.continuation? && !self.paragraph? && !self.procedure_division?
    end
    def blank?
      self.statement.empty?
    end
    def comment?
      self.control == '*'
    end
    def continuation?
      self.control == '-'
    end
    def paragraph?
      !self.area_a.strip.empty?
    end
    def paragraph_name
      self.statement.strip.delete '.'
    end
    def area_a
      self.statement[0..3]
    end
    def control
      @line.length > 6 ? @line[6] : ' '
    end
    def statement
      @line.length > 7 ? @line[7..@line.length] : ''
    end
    def procedure_division?
      @line.match /PROCEDURE DIVISION/
    end
  end

  class Paragraph
    attr_reader :name
    attr_accessor :lines
    def initialize name
      @name = name
      @lines = 0
    end
  end

end
