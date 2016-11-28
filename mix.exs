defmodule Wok.Rest.Mixfile do
  use Mix.Project

  def project do
    [
      app: :wok_rest,
      version: "0.1.0",
      elixir: "~> 1.2",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps,
      aliases: aliases
    ]
  end

  def application do
    [
       applications: [:syntax_tools, :compiler, :crypto, :goldrush, :lager],
       env: []
    ]
  end

  defp deps do
    [
      {:lager, "~> 3.2.0"},
      {:cowboy_default_static_file, git: "https://github.com/botsunit/cowboy_default_static_file.git", branch: "master"},
      {:cowboy, git: "https://github.com/ninenines/cowboy.git", tag: "2.0.0-pre.3"},
      {:bucs, "~> 1.0.2"},
      {:doteki, "~> 1.0.2"},
      {:ephemeral, "~> 2.0.0"},
      {:erlydtl, "~> 0.11.0", hex: :erlydtl2}    
    ]
  end

  defp aliases do
    [compile: &compile_with_hooks/1]
  end

  defp compile_with_hooks(args) do
    pre_compile_hooks()
    :ok = Mix.Task.run("compile", args)
    post_compile_hooks()
  end

  defp pre_compile_hooks() do
    run_hook_cmd [
    ]
  end

  defp post_compile_hooks() do
    run_hook_cmd [
    ]
  end

  defp run_hook_cmd(commands) do
    {_, os} = :os.type
    for command <- commands, do: (fn
      ({regex, cmd}) ->
         if Regex.match?(Regex.compile!(regex), Atom.to_string(os)) do
           Mix.Shell.cmd cmd, [], fn(x) -> Mix.Shell.IO.info(String.strip(x)) end
         end
      (cmd) ->
        Mix.Shell.cmd cmd, [], fn(x) -> Mix.Shell.IO.info(String.strip(x)) end
      end).(command)
  end    
end