require 'cobplexity'

describe Cobplexity::Analyzer do

  it 'returns an empty metrics array if no file is specified' do
    subject.analyze.count.should == 0
  end

  it "doesn't return metrics objects for files that don't exist" do
    subject.files << 'spec/samples/missing.cbl'
    subject.analyze.count.should == 0
  end

  it 'returns metrics objects for each paragraph in the specified file' do
    subject.files << 'spec/samples/test.cbl'
    subject.analyze.count.should == 2
  end

  it 'returns metrics objects for each paragraph for multiple files' do
    subject.files << 'spec/samples/test.cbl'
    subject.files << 'spec/samples/test.cbl'
    subject.analyze.count.should == 4
  end

  it 'returns metrics objects for each paragraph for wildcarded files' do
    subject.files << 'spec/samples/*.cbl'
    subject.analyze.count.should == 2
  end

  context "when analyzing a simple program" do

    before :each do
      subject.files << 'spec/samples/test.cbl'
    end
 
    it "returns size metric as lines of code for a paragraph" do
      subject.analyze[0].size.should == 2 
    end

    it "returns color metric as complexity for a paragraph" do
      subject.analyze[0].color.should == 2
    end

    it "adjusts color metric by threshold" do
      subject.threshold = 10
      subject.analyze[0].color.should == 8
    end

    it "sets group as highest level category" do
      subject.group = "foo"
      subject.analyze[0].categories[0].should == "foo"
    end

    it "defaults group as APPLICATION" do
      subject.analyze[0].categories[0].should == "APPLICATION"
    end

    it "sets filename as second level of categorization" do
      subject.analyze[0].categories[1].should == 'spec/samples/test.cbl'
    end

    it "sets paragraph as third level of categorization" do
      subject.analyze[0].categories[2].should == 'MAINLINE'
    end

  end

end
