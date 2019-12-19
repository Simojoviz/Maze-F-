(*
* LabProg2019 - Progetto di Programmazione a.a. 2019-20
* Maze.fs: maze
* (C) 2019 Alvise Spano' @ Universita' Ca' Foscari di Venezia
*)

module LabProg2019.Maze

open External
open Gfx
open System
open System.Text
open Engine


type CharInfo with
    /// Shortcut for creating a wall pixel.
    static member wall = pixel.create (Config.wall_pixel_char, Color.White)
    /// Shortcut for creating a path pixel.
    static member internal path = pixel.filled Color.Black
    /// Check whether this pixel is a wall.
    member this.isWall = this = pixel.wall

let W = 31
let H = 31

type direction = Up | Right | Down | Left

type state = {
    player: sprite
    }


// TODO: implement the maze type, its generation (task 1) and its automatic resolution (task 2)
type maze (w, h) (*as this*) = 

    let mazestruct = new image (w, h, Array.create (w*h) CharInfo.wall) 

    let removeWall (x, y)=
        mazestruct.[x, y] <- CharInfo.path

    let rndDir l = 
        let i = rnd_int 0 3
        List.item i l

    let isValidCell (x,y) = x>=1 && x<=(w-1) && y>=1 && y<=(h-1) && (mazestruct.[x , y] = CharInfo.wall)    

        
    
    // TODO: do not forget to call the generation function in your object initializer
    //do this.generate

    // TODO: start with implementing the generation
    member __.generate =
        let geneerateMaze =
            mazestruct.[1 , 1] <- CharInfo.path
            mazestruct.[w-1 , h-2] <- CharInfo.path
            let rec loop m X Y move =
                    let dir = rndDir [Up ; Right ; Down ; Left]
                    match dir with 
                    |Up when isValidCell (X , Y - 2) -> mazestruct.[X , Y-2] <- CharInfo.path
                                                        removeWall (X,Y-1)
                                                        loop mazestruct X (Y-2) (Up::move)
                    
                    |Right when isValidCell (X + 2 , Y) -> mazestruct.[X+2 , Y] <- CharInfo.path
                                                           removeWall (X+1,Y)                                                           
                                                           loop mazestruct (X+2) Y (Right::move)
                    
                    |Down when isValidCell (X , Y + 2) -> mazestruct.[X , Y+2] <- CharInfo.path
                                                          removeWall (X,Y+1)                                                          
                                                          loop mazestruct X (Y+2) (Down::move)
                    
                    |Left when isValidCell (X - 2 , Y) -> mazestruct.[X-2 , Y] <- CharInfo.path
                                                          removeWall (X-1,Y)                                                          
                                                          loop mazestruct (X-2) Y (Left::move)
                    |_ -> if isValidCell (X , Y - 2) || isValidCell (X + 2 , Y) || isValidCell (X , Y + 2) || isValidCell (X - 2 , Y) then loop m X Y move
                          else if move = [] then ()                           
                               else match List.head move with
                                    |Up -> loop mazestruct X (Y+2) (List.tail move)
                                    
                                    |Right -> loop mazestruct (X-2) Y (List.tail move)
                                    
                                    |Down -> loop mazestruct X (Y-2) (List.tail move)

                                    |Left ->  loop mazestruct (X+2) Y (List.tail move)
            loop mazestruct 1 1 []             
        geneerateMaze

    member val finalMaze = mazestruct

                       
        
 let main () = 
    
    let engine = new engine (W+2, H+3)

    let maze = new maze (W,H)
    maze.generate
    
    let update (key : ConsoleKeyInfo) (screen : wronly_raster) (st : state) =  
        let dx, dy =
            match key.KeyChar with 
            | 'w' -> if maze.finalMaze.[ (int st.player.x) - 2 , (int st.player.y) - 4] = CharInfo.path then 0. , -1.
                     else 0. , 0.
            | 's' -> if maze.finalMaze.[ (int st.player.x) - 2 , (int st.player.y) - 2 ] = CharInfo.path then 0. , +1.
                     else 0. , 0.                     
            | 'a' -> if maze.finalMaze.[ (int st.player.x) - 3 , (int st.player.y) - 3 ] = CharInfo.path then -1. , 0.
                     else 0. , 0.            
            | 'd' -> if maze.finalMaze.[ (int st.player.x) - 1 , (int st.player.y) - 3 ] = CharInfo.path then +1. , 0.
                     else 0. , 0.            
            | _   -> 0., 0.              
        st.player.move_by (dx, dy)
        st, key.KeyChar = 'q'

    ignore <| engine.create_and_register_sprite (maze.finalMaze, 2, 3, 0 )

    let player = engine.create_and_register_sprite (image.rectangle (1, 1, pixel.filled Color.Red), 3, 4, 1)

    let st0 = {
        player = player 
        }

    engine.loop_on_key update st0


  
  
