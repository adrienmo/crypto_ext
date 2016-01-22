defmodule CryptoExt.Mixfile do
  use Mix.Project

  @version File.read!("VERSION") |> String.strip

  def project do
    [app: :crypto_ext,
     version: @version,
     description: "Crypto lib extension to support AES 128 ECB cipher",
     package: package,
     deps: []]
  end

  defp package do
    [files: ~w(c_src src rebar.config README.md LICENSE VERSION),
     maintainers: ["Adrien Moreau"],
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/adrienmo/eredis_cluster"}]
  end
end
