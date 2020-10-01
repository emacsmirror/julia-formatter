# Copyright © 2020 Felipe Lema
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

using JSON
using JuliaFormatter
using CSTParser

@doc raw"""
Read a JSON method call from stream

Taken from https://github.com/julia-vscode/LanguageServer.jl/blob/f71cdb047b06be6c2ff74dbdd91e9f05127e20fa/src/jsonrpcendpoint.jl#L30
"""
function read_transport_layer(stream)
    header_dict = Dict{String,String}()
    line = chomp(readline(stream))
    while length(line) > 0
        h_parts = split(line, ":")
        header_dict[chomp(h_parts[1])] = chomp(h_parts[2])
        line = chomp(readline(stream))
    end
    message_length = parse(Int, header_dict["Content-Length"])
    message_str = String(read(stream, message_length))
    return message_str
end

@doc raw"""
Read "text" from JSON message and return formatted text as a list of lines.

"""
function format_data(rpc_message)
    original_lines = rpc_message["params"]["text"]
    text_to_format = join(original_lines, "\n")
    current_line = rpc_message["params"]["current_line"]
    original_current_line = string(original_lines[current_line])
    out_text = text_to_format
    if strip(text_to_format) ≠ ""
        out_text = format_text(
            text_to_format;
            # options that will not alter the number of lines in text
            # why? because altering the lines of text will confuse the users
            # since they're using this in live coding
            remove_extra_newlines=false,
            pipe_to_function_call=false,
            short_to_long_function_def=false,
            always_use_return=false,
            annotate_untyped_fields_with_any=false,
            always_for_in=false, # see Reactive.jl loops
        )
    end
    # split text into lines, right-stripped, corroctly indented
    lines = String[l for l in split(out_text, "\n")]
    if strip(original_current_line) == "" && current_line < length(lines)
        # it's very unconfortable to have the cursor move back to first column
        # of the line
        # so we'll keep the whitespace just for the line the user is standing on
        lines[current_line] = original_current_line
    end
    lines
end

@doc raw"""
Write JSON response to stream
Taken from https://github.com/julia-vscode/LanguageServer.jl/blob/f71cdb047b06be6c2ff74dbdd91e9f05127e20fa/src/jsonrpcendpoint.jl#L23
"""
function write_transport_layer(stream, response)
    response_utf8 = transcode(UInt8, response)
    n = length(response_utf8)
    write(stream, "Content-Length: $n\r\n\r\n")
    write(stream, response_utf8)
end

@doc raw"""
Read "text" and "position" from JSON message and return

"""
function defun_range(rpc_message)
    text_to_parse = join(rpc_message["params"]["text"], "\n")
    position = rpc_message["params"]["position"]
    defun_range(text_to_parse, position)
end

function defun_range(text_to_parse::AbstractString, position::Int64)
    root_expression = CSTParser.parse(text_to_parse, true)
    current_expression_start_position = 1
    for expression ∈ root_expression.args
        current_expression_end_position =
            expression.fullspan - 1 + current_expression_start_position
        if current_expression_start_position ≤ position ≤ current_expression_end_position
            return current_expression_start_position, current_expression_end_position
        end
        # next loop, expression should start just after current expression
        current_expression_start_position = current_expression_end_position + 1
    end
    @error string("Could not find any defun surrounding", "$(position)", "-th byte in text")
end

@doc raw"""
Call "method" from rpc_message, pack the result into a json response and return it
"""
function dispatch_response(rpc_message)
    response = Dict{String,Any}("jsonrpc" => "2.0", "id" => rpc_message["id"])
    try
        result = nothing
        if "format" == rpc_message["method"]
            response["result"] = format_data(rpc_message)
        elseif "defun_range" == rpc_message["method"]
            response["result"] = defun_range(rpc_message)
        else
            # this error is catched just below, mind you
            @error string("Unknown method ", rpc_message["method"])
        end
    catch err
        response["error"] = Dict("code" => 0, "message" => string(err))
    end
    return JSON.json(response)

end

@doc raw"""
Use json-rpc with stdin / stdout.

API is same as in JuliaFormatter.vim:

in:
    {… "method":"format", {"params" : {"text" : ["…", …] /* array of text lines */}, {"current_line": 10}} }

out:
    {… "result": ["…"] /* correctly formatted text lines */ }

in:
    {… "method":"defun_range", {"params" : {"text" : ["…", …] /* array of text lines */, "position" : 1 /* 1-based byte position */ }}}

out:
    {… "result": [10, 15] /* 1-based byte position */ }

This function was based on https://github.com/kdheepak/JuliaFormatter.vim/blob/02f0e67f9be07300b70d598a2119af8f915b2143/scripts/server.jl
"""
function run_server(instream, outstream)
    while true
        text = read_transport_layer(instream)
        rpc_message = JSON.parse(String(text))
        response = dispatch_response(rpc_message)
        write_transport_layer(outstream, response)
    end
end


# force code compilation just before setting up the server
format_text("Channel()")
defun_range("Channel()", 2)

run_server(stdin, stdout)

