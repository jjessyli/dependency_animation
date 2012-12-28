-- CIS 552 Project
-- Junyi Li (ljunyi), Chenhui Zhai (chenhuiz)
-- Input: an article text file, a dependency parsed file of the same article
-- Output: word visualization based on word weights and word relationship

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Simulate
import System.IO

import WordProcess
import Animation


main :: IO ()
main = do
       putStrLn "This is the mighty Lord Voldemort. Give me a muggle article to see through!"
       fname <- getLine
       fcontents <- readFile fname
       wordWeights <- makeWeightMap fcontents -- get word weights from article
       putStrLn "Give me a parsed file!"
       pname <- getLine
       phandle <- openFile pname ReadMode
       allEdgeStr <- getAllEdgeStr phandle -- get edge information
       wordEdges <- getAllEdges allEdgeStr wordWeights

       simulateInWindow
	"Voldemort's Eye"  -- window name
	(800, 800)      -- window size
	(10, 10)	-- window position
	(greyN 0.1)	-- background color
	2               -- number of steps per second
	(initWorld $ findMaxWeight wordWeights)  -- initial world
	render          -- function to convert world to a Picture
	(evolve wordWeights wordEdges)   -- function to step the world one iteration




