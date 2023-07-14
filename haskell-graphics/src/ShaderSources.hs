module ShaderSources (vertexShaderSource, fragmentShaderSource) where
vertexShaderSource = "#version 330 core\n\
    \layout (location = 0) in vec3 position;\
    \void main()\
    \{\
    \    gl_Position = vec4(position.x, position.y, position.z, 1.0);\
    \}" 

fragmentShaderSource = unlines ["#version 330 core",
    "out vec4 FragColor;",
    "",
    "void main()",
    "{",
    "    FragColor = vec4(1.0f, 0.5f, 0.2f, 1.0f);",
    "}"]

