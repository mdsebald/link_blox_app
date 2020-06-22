defmodule LinkBloxApp.MixProject do
  use Mix.Project

  @target System.get_env("MIX_TARGET") || "host"

  def project do
    [
      app: :link_blox_app,
      version: "0.6.0",
      elixir: "~> 1.10",
      language: :erlang,
      erlc_options: erlc_options(@target),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      preferred_cli_env: [eunit: :test]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      mod: {:link_blox_app, [:LinkBlox, :lang_en_us, 1111, :debug, :LinkBlox_cookie]}
      # extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:link_blox_cmn, github: "mdsebald/link_blox_cmn"},
      {:circuits_i2c, "~>0.3"},
      {:circuits_gpio, "~>0.4"},
      {:circuits_spi, "~>0.1"},
      {:emqtt, github: "emqx/emqtt"},
      {:lager, github: "basho/lager", branch: "master"},
      # 01-May-2018: hex.pm package still had some warnings. Using github version for now
      {:mix_eunit, github: "dantswain/mix_eunit", only: [:dev], runtime: false},
      {:ex_doc, "~> 0.11", only: :dev, runtime: false},
      {:dialyxir, "1.0.0-rc.4", only: :dev, runtime: false}
    ]
  end

  defp erlc_options("host") do
    if Mix.env() == :test do
      [{:d, :TEST}]
    else
      []
    end
  end

  defp erlc_options(_target) do
    [{:d, :STANDALONE}]
  end
end
