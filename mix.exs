defmodule CryptoExt.Mixfile do
  use Mix.Project

  @version File.read!("VERSION") |> String.strip

  def project do
    [app: :crypto_ext,
     version: @version,
     description: "Crypto lib extension to support AES 128 ECB cipher",
     compilers: [:crypto_ext_nifs] ++ Mix.compilers,
     package: package,
     deps: [{:ex_doc, ">= 0.0.0", only: :dev}]]
  end

  defp package do
    [files: ~w(c_src src rebar.config README.md LICENSE VERSION),
     maintainers: ["Adrien Moreau"],
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/adrienmo/crypto_ext"}]
  end
end

defmodule Mix.Tasks.Compile.CryptoExtNifs do
  def run(_args) do
     system_version = :erlang.system_info(:system_architecture) |> to_string
     cmd =
       cond do
         Regex.match?(~r/linux|darwin|solaris/, system_version) -> "make"
         Regex.match?(~r/freebsd/, system_version) -> "gmake"
         true -> "make"
        end
       {result, _errcode} = System.cmd(cmd, ["-C", "c_src"], stderr_to_stdout: true)
       IO.binwrite(result)
  end
end
