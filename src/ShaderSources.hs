module ShaderSources (vertexShaderSource, vertexShaderSourceTransform, fragmentShaderSourceBlue, fragmentShaderSourceBlack, fragmentShaderSourceRed, fragmentShaderSourcePulse, fragmentShaderSourceColor ) where
vertexShaderSource = "#version 330 core\n\
    \layout (location = 0) in vec3 position;\
    \void main()\
    \{\
    \    gl_Position = vec4(position.x, position.y, position.z, 1.0);\
    \}" 

vertexShaderSourceTransform = "#version 330 core\n\
    \layout (location = 0) in vec3 position;\
    \layout (location = 1) in vec2 aTexCoord;\
    \out vec3 ourColor;\
    \uniform mat4 transform1;\
    \uniform mat4 projection;\

    \void main()\
    \{\
    \    gl_Position = projection*transform1 * vec4(position.x, position.y, position.z, 1.0);\
    \    ourColor = vec3(position.x + 0.1, position.y + 0.1, position.z + 0.1);\
    \}" 

fragmentShaderSourceColor = unlines ["#version 330 core",
    "out vec4 FragColor;",
    "",
    "uniform vec4 backgroundColor;",
    "void main()",
    "{",
    "    FragColor = backgroundColor;",
    "}"]

fragmentShaderSourceBlue = unlines ["#version 330 core",
    "out vec4 FragColor;",
    "",
    "void main()",
    "{",
    "    FragColor = vec4(0.0f, 0.0f, 1.0f, 0.0f);",
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
    "    FragColor = vec4(1.0f, 1.0f, 1.0f, 0.5f);",
    "}"]


fragmentShaderSourcePulse = unlines ["#version 330 core",
    "out vec4 FragColor;",
    "",
    "uniform vec4 ourColor;",
    "void main()",
    "{",
    "    FragColor = ourColor;",
    "}"
    ]