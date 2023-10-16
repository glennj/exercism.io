require "spec"
require "../src/*"

describe "RobotSimulator" do
  it "at origin facing north" do
    robot = RobotSimulator.new({0, 0}, :north)

    robot.x.should eq 0
    robot.y.should eq 0
    robot.direction.should eq :north
  end

  pending "at negative position facing south" do
    robot = RobotSimulator.new({-1, -1}, :south)

    robot.x.should eq -1
    robot.y.should eq -1
    robot.direction.should eq :south
  end

  pending "changes north to east" do
    robot = RobotSimulator.new({0, 0}, :north)
    robot.move("R")

    robot.x.should eq 0
    robot.y.should eq 0
    robot.direction.should eq :east
  end

  pending "changes east to south" do
    robot = RobotSimulator.new({0, 0}, :east)
    robot.move("R")

    robot.x.should eq 0
    robot.y.should eq 0
    robot.direction.should eq :south
  end

  pending "changes south to west" do
    robot = RobotSimulator.new({0, 0}, :south)
    robot.move("R")

    robot.x.should eq 0
    robot.y.should eq 0
    robot.direction.should eq :west
  end

  pending "changes west to north" do
    robot = RobotSimulator.new({0, 0}, :west)
    robot.move("R")

    robot.x.should eq 0
    robot.y.should eq 0
    robot.direction.should eq :north
  end

  pending "changes north to west" do
    robot = RobotSimulator.new({0, 0}, :north)
    robot.move("L")

    robot.x.should eq 0
    robot.y.should eq 0
    robot.direction.should eq :west
  end

  pending "changes west to south" do
    robot = RobotSimulator.new({0, 0}, :west)
    robot.move("L")

    robot.x.should eq 0
    robot.y.should eq 0
    robot.direction.should eq :south
  end

  pending "changes south to east" do
    robot = RobotSimulator.new({0, 0}, :south)
    robot.move("L")

    robot.x.should eq 0
    robot.y.should eq 0
    robot.direction.should eq :east
  end

  pending "changes east to north" do
    robot = RobotSimulator.new({0, 0}, :east)
    robot.move("L")

    robot.x.should eq 0
    robot.y.should eq 0
    robot.direction.should eq :north
  end

  pending "facing north increments Y" do
    robot = RobotSimulator.new({0, 0}, :north)
    robot.move("A")

    robot.x.should eq 0
    robot.y.should eq 1
    robot.direction.should eq :north
  end

  pending "facing south decrements Y" do
    robot = RobotSimulator.new({0, 0}, :south)
    robot.move("A")

    robot.x.should eq 0
    robot.y.should eq -1
    robot.direction.should eq :south
  end

  pending "facing east increments X" do
    robot = RobotSimulator.new({0, 0}, :east)
    robot.move("A")

    robot.x.should eq 1
    robot.y.should eq 0
    robot.direction.should eq :east
  end

  pending "facing west decrements X" do
    robot = RobotSimulator.new({0, 0}, :west)
    robot.move("A")

    robot.x.should eq -1
    robot.y.should eq 0
    robot.direction.should eq :west
  end

  pending "moving east and north from README" do
    robot = RobotSimulator.new({7, 3}, :north)
    robot.move("RAALAL")

    robot.x.should eq 9
    robot.y.should eq 4
    robot.direction.should eq :west
  end

  pending "moving west and north" do
    robot = RobotSimulator.new({0, 0}, :north)
    robot.move("LAAARALA")

    robot.x.should eq -4
    robot.y.should eq 1
    robot.direction.should eq :west
  end

  pending "moving west and south" do
    robot = RobotSimulator.new({2, -7}, :east)
    robot.move("RRAAAAALA")

    robot.x.should eq -3
    robot.y.should eq -8
    robot.direction.should eq :south
  end

  pending "moving east and north" do
    robot = RobotSimulator.new({8, 4}, :south)
    robot.move("LAAARRRALLLL")

    robot.x.should eq 11
    robot.y.should eq 5
    robot.direction.should eq :north
  end
end
