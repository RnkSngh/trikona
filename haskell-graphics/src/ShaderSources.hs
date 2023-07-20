module ShaderSources (vertexShaderSource, vertexShaderSourceTransform, fragmentShaderSourceBlue, fragmentShaderSourceBlack, fragmentShaderSourceRed, fragmentShaderSourcePulse) where
vertexShaderSource = "#version 330 core\n\
    \layout (location = 0) in vec3 position;\
    \void main()\
    \{\
    \    gl_Position = vec4(position.x, position.y, position.z, 1.0);\
    \}" 

vertexShaderSourceTransform = "#version 330 core\n\
    \layout (location = 0) in vec3 position;\
    \layout (location = 1) in vec2 aTexCoord;\
    \out vec2 TexCoord;\
    \uniform mat4 transform1;\

    \void main()\
    \{\
    \    gl_Position = transform1 * vec4(position.x, position.y, position.z, 1.0);\
    \    TexCoord = vec2(aTexCoord.x, aTexCoord.y);\
    \}" 

-- vertexShaderSourceTransform = "#version 330 core\n\
--     \layout (location = 0) in vec3 aPos;\
--     \layout (location = 1) in vec2 aTexCoord;\
--     \out vec2 TexCoord;\
--     \uniform mat4 transform;\
--     \void main()\
--     \{\
--     \    gl_Position = transform * vec4(aPos, 1.0f);\
--     \    TexCoord = vec2(aTexCoord.x, aTexCoord.y);\
--     \}"


-- vertexShaderSourceTransform = unlines [""]

fragmentShaderSourceBlue = unlines ["#version 330 core",
    "out vec4 FragColor;",
    "",
    "uniform mat4 transform;",
    "void main()",
    "{",
    "    FragColor = vec4(0.0f, 0.0f, 1.0f, 1.0f);",
    "}"]

fragmentShaderSourceBlack = unlines ["#version 330 core",
    "out vec4 FragColor;",
    "",
    "uniform mat4 transform;",
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


fragmentShaderSourcePulse = unlines ["#version 330 core",
    "out vec4 FragColor;",
    "",
    "uniform vec4 ourColor;",
    "void main()",
    "{",
    "    FragColor = ourColor;",
    "}"
    ]