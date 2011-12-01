module Cobplexity

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
    def branches
      self.statement.split.count do |item|
        ['IF', 'ELSE', 'WHEN', 'WHILE', 'UNTIL'].include? item.upcase
      end
    end
    def paragraph?
      !self.statement.match(/COPY /i) && !self.area_a.strip.empty?
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
      @line.match /PROCEDURE DIVISION/i
    end
  end

end
