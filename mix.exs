defmodule LinkBloxApp.MixProject do
  use Mix.Project

  def project do
    [
      app: :link_blox_app,
      version: "0.5.0",
      elixir: "~> 1.6",
      language: :erlang,
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      preferred_cli_env: [eunit: :test]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      mod: {:link_blox_app, [:LinkBlox, :lang_en_us, 1111, :debug, :LinkBlox_cookie]},
      # extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:erlang_ale, github: "esl/erlang_ale"},
      {:emqttc, github: "emqtt/emqttc"},
      {:lager, github: "basho/lager", branch: "master"},
      # 01-May-2018: hex.pm package still had some warnings. Using github version for now
      {:mix_eunit, github: "dantswain/mix_eunit", only: [:dev], runtime: false},
      {:dialyxir, "~> 0.5", only: [:dev], runtime: false}
    ]
  end
end
