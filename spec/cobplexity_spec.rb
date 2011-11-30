require 'cobplexity'

describe Module do

  it 'has code' do
    subject.code = 'foo'
    subject.code.should == 'foo'
  end

  it 'counts lines of code' do
    subject.code = <<-eos
100000     MOVE 'Y' to YES.
100010     CALL MOVE-IT.
100020     CALL MOVE-IT-AGAIN.
    eos
    subject.lines.should == 3
  end

  it "doesn't count blank lines" do
    subject.code = <<-eos
100000     MOVE 'Y' to YES.

100020     CALL MOVE-IT-AGAIN.
    eos
    subject.lines.should == 2
  end

  it "doesn't counts lines with only a line number" do
    subject.code = <<-eos
100000     MOVE 'Y' to YES.
100010
100020     CALL MOVE-IT-AGAIN.
    eos
    subject.lines.should == 2
  end

end
