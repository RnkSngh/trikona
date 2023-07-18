module ShaderSources (vertexShaderSource, fragmentShaderSourceBlue, fragmentShaderSourceBlack, fragmentShaderSourceRed) where
vertexShaderSource = "#version 330 core\n\
    \layout (location = 0) in vec3 position;\
    \void main()\
    \{\
    \    gl_Position = vec4(position.x, position.y, position.z, 1.0);\
    \}" 

fragmentShaderSourceBlue = unlines ["#version 330 core",
    "out vec4 FragColor;",
    "",
    "void main()",
    "{",
    "    FragColor = vec4(0.0f, 0.0f, 1.0f, 1.0f);",
    "}"]


fragmentShaderSourceBlack = unlines ["#version 330 core",
    "out vec4 FragColor;",
    "",
    "void main()",
    "{",
    "    FragColor = vec4(0.0f, 0.0f, 0.0f, 1.0f);",
    "}"]



fragmentShaderSourceRed = unlines ["#version 330 core",
    "out vec4 FragColor;",
    "",
    "void main()",
    "{",
    "    FragColor = vec4(1.0f, 0.0f, 0.0f, 1.0f);",
    "}"]
