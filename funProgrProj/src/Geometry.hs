module Geometry where

-- Define your data types and functions
data GeomExp = NoPoints
             | Point Double Double
             | Line Double Double
             | VerticalLine Double
             | LineSegment Double Double Double Double
             | Intersect GeomExp GeomExp
             | Let String GeomExp GeomExp
             | Var String
             | Shift Double Double GeomExp
             deriving (Show, Eq)

-- Function to preprocess expressions
preprocessProg :: GeomExp -> GeomExp
preprocessProg (LineSegment x1 y1 x2 y2)
  | realClose x1 x2 && realClose y1 y2 = Point x1 y1
  | x1 > x2 || (realClose x1 x2 && y1 > y2) = LineSegment x2 y2 x1 y1
  | otherwise = LineSegment x1 y1 x2 y2
preprocessProg (Intersect e1 e2) = Intersect (preprocessProg e1) (preprocessProg e2)
preprocessProg (Let var e1 e2) = Let var (preprocessProg e1) (preprocessProg e2)
preprocessProg (Shift dx dy e) = Shift dx dy (preprocessProg e)
preprocessProg other = other

-- Function to evaluate expressions
evalProg :: GeomExp -> [(String, GeomExp)] -> GeomExp
evalProg NoPoints _ = NoPoints
evalProg (Point x y) _ = Point x y
evalProg (Line m b) _ = Line m b
evalProg (VerticalLine x) _ = VerticalLine x
evalProg (LineSegment x1 y1 x2 y2) _ = LineSegment x1 y1 x2 y2
evalProg (Intersect e1 e2) env = intersect (evalProg e1 env) (evalProg e2 env)
evalProg (Let var e1 e2) env = evalProg e2 ((var, evalProg e1 env) : env)
evalProg (Var var) env = case lookup var env of
                            Just val -> val
                            Nothing -> error ("Variable " ++ var ++ " not found")
evalProg (Shift dx dy e) env = shift dx dy (evalProg e env)

-- Function to compute the intersection of two geometric expressions
intersect :: GeomExp -> GeomExp -> GeomExp
intersect NoPoints _ = NoPoints
intersect _ NoPoints = NoPoints
intersect (Point x1 y1) (Point x2 y2)
  | realClose x1 x2 && realClose y1 y2 = Point x1 y1
  | otherwise = NoPoints
intersect (Line m1 b1) (Line m2 b2)
  | realClose m1 m2 && realClose b1 b2 = Line m1 b1
  | realClose m1 m2 = NoPoints
  | otherwise = Point x y
  where
    x = (b2 - b1) / (m1 - m2)
    y = m1 * x + b1
intersect (VerticalLine x1) (VerticalLine x2)
  | realClose x1 x2 = VerticalLine x1
  | otherwise = NoPoints
intersect (VerticalLine x1) (Line m b) = Point x1 (m * x1 + b)
intersect (Line m b) (VerticalLine x1) = Point x1 (m * x1 + b)
intersect (LineSegment x1 y1 x2 y2) (LineSegment x3 y3 x4 y4) = intersectLineSegments (LineSegment x1 y1 x2 y2) (LineSegment x3 y3 x4 y4)
intersect _ _ = NoPoints  -- Handle other cases appropriately

-- Helper function for intersecting line segments (to be implemented)
intersectLineSegments :: GeomExp -> GeomExp -> GeomExp
intersectLineSegments (LineSegment x1 y1 x2 y2) (LineSegment x3 y3 x4 y4) = undefined

-- Function to shift geometric expressions
shift :: Double -> Double -> GeomExp -> GeomExp
shift dx dy NoPoints = NoPoints
shift dx dy (Point x y) = Point (x + dx) (y + dy)
shift dx dy (Line m b) = Line m (b + dy - m * dx)
shift dx dy (VerticalLine x) = VerticalLine (x + dx)
shift dx dy (LineSegment x1 y1 x2 y2) = LineSegment (x1 + dx) (y1 + dy) (x2 + dx) (y2 + dy)
shift dx dy (Intersect e1 e2) = Intersect (shift dx dy e1) (shift dx dy e2)
shift dx dy (Let var e1 e2) = Let var (shift dx dy e1) e2
shift dx dy (Var var) = Var var

-- Helper function for floating-point comparison
realClose :: Double -> Double -> Bool
realClose x y = abs (x - y) < 0.00001
