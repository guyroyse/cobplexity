module Cobplexity

  class Module
    attr_reader :code, :lines
    def code= code
      @code = code
      @lines = 0
      @code.lines.each do |line|
        cobol_line = Line.new line
        @lines = 0 if cobol_line.procedure_division?
        @lines+= 1 if cobol_line.code?
      end
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

end
