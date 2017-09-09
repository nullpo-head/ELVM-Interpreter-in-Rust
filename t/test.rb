#!/usr/bin/ruby

require "test/unit"

class TestInterpreter < Test::Unit::TestCase

  BIN = "../target/release/rust_elvmi"
  TESTDIR = "./test"

  Dir.glob("#{TESTDIR}/*.eir").each do |filename|
    define_method(("test_" + File.basename(filename, ".eir")).to_sym) do
      inputs = Dir.glob("#{TESTDIR}/*.in").select {|name| name.match(filename.sub(/(\.c|\.eir)+$/, "(_[0-9]+)?.in"))}
      if inputs.length > 0
        test_result = ""
        inputs.each do |input|
          test_result << "=== test/#{File.basename(input)} ===\n" 
          test_result << `#{BIN} #{filename} < #{input}` + "\n"
        end
      else
        test_result = `#{BIN} #{filename}`
      end
      assert_equal File.read(filename + ".out"), test_result
    end
  end

end
