module Cobplexity

  class Analyzer
    attr_accessor :files, :threshold, :group
    def initialize
      @files = []
      @threshold = 0
      @group = "APPLICATION"
      @metrics = []
    end
    def analyze
      @files.each do |file|
        Dir.glob file do |filename|
          @filename = filename
          analyze_file
        end
      end
      @metrics
    end
    def analyze_file
      File.open @filename do |file|
        program = Module.new file.readlines.join
        program.paragraphs.each do |paragraph|
          analyze_paragraph paragraph
        end
      end
    end
    def analyze_paragraph paragraph
      metric = Metric.new 
      metric.size = paragraph.lines
      metric.color = @threshold == 0 ? paragraph.complexity : @threshold - paragraph.complexity
      metric.categories << @group
      metric.categories << @filename
      metric.categories << paragraph.name
      @metrics << metric
    end
  end

  class Metric
    attr_accessor :size, :color, :categories
    def initialize
      @categories = []
    end
    def to_s
      categories = @categories.collect do |category|
        category.delete '",'
      end
      "#{@size},#{@color},#{categories.join ','}"
    end
  end

end
