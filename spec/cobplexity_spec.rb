require 'cobplexity'

describe Module do

  it 'counts lines of code' do
    subject.code = <<-eos
100000     MOVE 'Y' to YES.
100010     CALL MOVE-IT.
    eos
    subject.lines.should == 2
  end

end
