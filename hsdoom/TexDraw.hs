module TexDraw (
    TexturedQuad(..),
    drawTexturedQuad
    )
    where 

import Graphics.Rendering.OpenGL

data TexturedQuad = TexturedQuad
    { _rx1, _ry1, _rz1
    , _rx2, _ry2, _rz2
    , _rx3, _ry3, _rz3
    , _rx4, _ry4, _rz4
    , _tx1, _tx2, _ty1, _ty2 :: GLfloat
    }

drawTexturedQuad :: TexturedQuad -> IO ()
drawTexturedQuad (TexturedQuad x1 y1 z1 x2 y2 z2
                               x3 y3 z3 x4 y4 z4
                               tx1 tx2 ty1 ty2) = do
  renderPrimitive Quads $ do
    texCoord  $ TexCoord2 tx1 ty1
    vertex    $ Vertex3   x1 y1 z1
    texCoord  $ TexCoord2 tx1 ty2
    vertex    $ Vertex3   x2 y2 z2
    texCoord  $ TexCoord2 tx2 ty2
    vertex    $ Vertex3   x3 y3 z3
    texCoord  $ TexCoord2 tx2 ty1
    vertex    $ Vertex3   x4 y4 z4