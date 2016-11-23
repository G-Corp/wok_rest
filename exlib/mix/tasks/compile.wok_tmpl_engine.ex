defmodule Mix.Tasks.Compile.WokTmplEngine do
  use Mix.Task
  alias Mix.Compilers.Erlang
  import Mix.Compilers.Erlang

  @recursive true
  @manifest ".compile.dtl"
  @default_compiler_options [
    {:libraries, [{:wok_helpers, :wok_helpers_dtl}]},
    {:default_libraries, [:wok_helpers]},
    :report_warnings,
    :report_errors,
    :warnings_as_errors
  ]
  @default_source_option "templates"
  @default_ext_option :tmpl
  @default_suffix_option "_tmpl"
  @default_full_path_option true
  @default_options [
    source: @default_source_option,
    ext: @default_ext_option,
    suffix: @default_suffix_option,
    compiler_options: @default_compiler_options,
    full_path: @default_full_path_option
  ]
  @default_force_option false

  @spec run(OptionParser.argv) :: :ok | :noop
  def run(args) do
    {opts, _, _} = OptionParser.parse(args, switches: [force: :boolean])

    project = Mix.Project.config
    erlydtl_options = project[:erlydtl_options] || @default_options
    compiler_options = erlydtl_options[:compiler_options] || @default_compiler_options
    templates_ext = erlydtl_options[:ext] || @default_ext_option
    manifest = Path.join Mix.Project.manifest_path, ".compile." <> Atom.to_string(templates_ext)
    source_path = Path.expand(erlydtl_options[:source] || @default_source_option) <> "/"
    full_path = erlydtl_options[:full_path] || @default_full_path_option
    template_suffix = erlydtl_options[:suffix] || @default_suffix_option
    force = opts[:force] || erlydtl_options[:force] || @default_force_option
    dest_path = to_erl_file Mix.Project.compile_path(project)
    mappings = [{source_path, dest_path}]

    Erlang.compile(manifest, mappings, templates_ext, :beam, force, fn(input, _output) ->
      module = if full_path == true do
        input
        |> String.replace(source_path, "")
        |> String.replace("/", "_")
      else
        input
        |> Path.basename
      end
      |> Path.rootname
      |> String.replace(".", "_")
      |> fucking_join(template_suffix)
      |> String.to_atom
      case :erlydtl.compile_file(String.to_char_list(input), module, [out_dir: dest_path] ++ compiler_options) do
        {:ok, _} -> {:ok, module}
        {:ok, _, _} -> {:ok, module}
        {:ok, _, _, _} -> {:ok, module}
        {:error, _errors, _warnings} -> :error
        :error -> :error
      end
    end)
  end

  def manifests, do: [manifest]
  defp manifest, do: Path.join(Mix.Project.manifest_path, @manifest)

  def clean do
    Erlang.clean(manifest())
  end

  def module_from_artifact(artifact) do
    name = artifact |> Path.basename |> Path.rootname
    name <> "_dtl"
  end

  def fucking_join(prefix, suffix) do
    prefix <> suffix
  end
end
