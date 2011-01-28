module B1.Program.Chart.SideBar
  ( SideBarInput(..)
  , SideBarOutput(..)
  , drawSideBar
  ) where

import Graphics.Rendering.OpenGL

import B1.Graphics.Rendering.OpenGL.Shapes
import B1.Graphics.Rendering.OpenGL.Utils
import B1.Program.Chart.Dirty
import B1.Program.Chart.Resources

data SideBarInput = SideBarInput

data SideBarOutput = SideBarOutput
  { isDirty :: Dirty
  }

drawSideBar :: Resources -> SideBarInput -> IO SideBarOutput
drawSideBar resources _ = do
  let sideBarHeight = realToFrac (windowHeight resources)

  loadIdentity
  translate $ vector3 (sideBarWidth resources / 2) (sideBarHeight / 2) 0
  scale3 (sideBarWidth resources / 2) (sideBarHeight / 2) 1
  color $ color4 0 0 1 0.5
  drawSquarePlaceholder

  return SideBarOutput { isDirty = False }

