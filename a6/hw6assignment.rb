# Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in,
# so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here:
  All_My_Pieces = [
                    [[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
                    rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
                    [[[0, 0], [-1, 0], [1, 0], [2, 0]], # 4 long (only needs two)
                      [[0, 0], [0, -1], [0, 1], [0, 2]]], 
                    rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
                    rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
                    rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
                    rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
                    [[[0, 0], [-1, 0], [-2, 0], [1, 0], [2, 0]], # 5 long (only needs two)
                      [[0, 0], [0, -1], [0, -2], [0, 1], [0, 2]]],
                    rotations([[0, 0], [1, 0], [0, -1]]), # 3 piece L
                    rotations([[0, 0], [1, 0], [0, 1], [1, 1], [0, 2]]) # square + plus one
                  ] 

  # Your Enhancements here
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.cheat_piece (board)
    MyPiece.new([[[0, 0]]], board)
  end
  
end

class MyBoard < Board
  # Your Enhancements here:
  def initialize (game)
    super
    @cheated = 0
  end

  # rotate piece 180 degrees
  def rotate_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  # rotate piece 180 degrees
  def cheat
    if !game_over? and @game.is_running? and @score > 100 and @cheated == 0
      @score = @score - 100
      @cheated = 1
    end
  end

  def next_piece
    if @cheated == 1
      @current_block = MyPiece.cheat_piece(self) 
      @cheated = 0
    else 
      @current_block = MyPiece.next_piece(self)
    end 
    @current_pos = nil
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    block_size = (locations.size - 1)
    (0..block_size).each { |index|
      current = locations[index];
      @grid[current[1] + displacement[1]][current[0] + displacement[0]] = @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
end

class MyTetris < Tetris
  # Your Enhancements here:
  # creates a canvas and the board that interacts with it
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  # extend key_bindings method
  def key_bindings   
    super
    @root.bind('u' , proc {@board.rotate_180})
    @root.bind('c' , proc {@board.cheat})
  end
end
