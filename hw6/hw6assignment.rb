# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
                   rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
                   [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
                    [[0, 0], [0, -1], [0, 1], [0, 2]]],
                   rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
                   rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
                   rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
                   rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
                   [[[0, 0],[-1, 0],[1, 0],[2, 0],[3, 0]],
                    [[0, 0],[0, -1],[0, 1],[0, 2],[0, 3]]],  #long 5
                   rotations([[0, 0], [1, 0], [0, 1], [1, 1], [2, 0]]), #other 5
                   rotations([[0, 0], [1, 0], [0, 1]])] #triple L

  Cheat_Piece = [[0, 0]]

  
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end
  
  def self.cheat_piece (board)
    MyPiece.new(Cheat_Piece, board)
  end

end

class MyBoard < Board
  
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
    @cheat = false
  end

  def cheat_fun
    if !@cheat and score >= 100
      @cheat = true
      @score = (@score - 100)
    end
  end

  def next_piece
    if @cheat
      @current_block = MyPiece.cheat_piece(self)
      @cheat = false
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end
  
  def rotate_one_eighty
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 1)
      @current_block.move(0, 0, 1)
    end
    draw
  end
  
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(locations.size - 1)).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
        @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
  
end

class MyTetris < Tetris
  # your enhancements here
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  
  def key_bindings  
    @root.bind('n', proc {self.new_game}) 

    @root.bind('p', proc {self.pause}) 

    @root.bind('q', proc {exitProgram})
    
    @root.bind('a', proc {@board.move_left})
    @root.bind('Left', proc {@board.move_left}) 
    
    @root.bind('d', proc {@board.move_right})
    @root.bind('Right', proc {@board.move_right}) 

    @root.bind('s', proc {@board.rotate_clockwise})
    @root.bind('Down', proc {@board.rotate_clockwise})

    @root.bind('w', proc {@board.rotate_counter_clockwise})
    @root.bind('Up', proc {@board.rotate_counter_clockwise}) 
    
    @root.bind('space' , proc {@board.drop_all_the_way})
    
    @root.bind('u' , proc {@board.rotate_one_eighty})
    @root.bind('c', proc {@board.cheat_fun})
  end


end





############################## CHALLENGE ###############################
class MyChallengePiece < MyPiece
  
  def self.next_piece (board)
    MyChallengePiece.new(All_My_Pieces.sample, board)
  end
  
  def self.cheat_piece (board)
    MyChallengePiece.new(Cheat_Piece, board)
  end

  def drop_by_one
    @moved = move(0, 1, 0)
  end

  def drop_by_two
    @moved = move(0, 2, 0)
  end
  
  
end


class MyChallengeBoard < MyBoard

  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyChallengePiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
    @cheat = false
    @speed = false
  end
  
  def run
    if !@speed
      ran = @current_block.drop_by_one
    else
      ran = @current_block.drop_by_two
    end
    if !ran
      store_current
      if !game_over?
        next_piece
      end
    end
    @game.update_score
    @game.update_speed
    
    draw
  end

  def next_piece
    if @cheat
      @current_block = MyChallengePiece.cheat_piece(self)
      @cheat = false
    else
      @current_block = MyChallengePiece.next_piece(self)
    end
    @current_pos = nil
  end

  def speed
    if !@speed
      @speed = true
      @score = @score + 100
    else
      @speed = false
      @score = @score - 100
    end
  end

  def get_speed
    @speed
  end
  
  
end

class MyChallengeTetris < MyTetris
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyChallengeBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def run_game
    if !@board.game_over? and @running
      @timer.stop
      @timer.start(@board.delay, (proc{@board.run; run_game}))
    else
      label = TetrisLabel.new(@root) do
        text 'GAME OVER!
  Final Score:  '
        background 'red'
      end
      label.place(35, 90, 60, 250)

      final_score_label = TetrisLabel.new(@root) do
        background 'red'
      end
      final_score_label.text(@board.score)
      final_score_label.place(15, 90, 60, 285)
      
    end
  end

  def update_speed
    if (@board.get_speed)
      @speed.text("ON")
      
    else
      @speed.text("OFF")
      
    end
  end

  def buttons
    super
    label = TetrisLabel.new(@root) do
      text 'Speed 
Mode: '
      background 'lightblue'
    end
    label.place(35, 90, -20, 495)

    @speed = TetrisLabel.new(@root) do
      background 'red'
    end
    @speed.text(@board.get_speed)
    @speed.place(35, 30, 45, 495)

    
  end

  def key_bindings
    super
    @root.bind('f', proc {@board.speed})
    
  end
  
end

#MAKE GAME END MESSAGE
