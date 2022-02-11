defmodule BankAccount do
  use Agent

  @moduledoc """
  A bank account that supports access from multiple processes.
  """

  # a bank account structure
  defstruct status: :open, balance: 0

  @closed_account_error {:error, :account_closed}

  @typedoc """
  An account handle.
  """
  @opaque account :: pid

  @doc """
  Open the bank. Makes the account available.
  """
  @spec open_bank() :: account
  def open_bank() do
    {:ok, pid} = Agent.start_link(fn -> %BankAccount{} end)
    pid
  end

  @doc """
  Close the bank. Makes the account unavailable.
  """
  @spec close_bank(account) :: none
  def close_bank(account) do
    Agent.update(account, fn acct ->
      %{acct | :status => :closed}
    end)
  end

  @doc """
  Get the account's balance.
  """
  @spec balance(account) :: integer
  def balance(account) do
    acct = Agent.get(account, & &1)

    case acct.status do
      :closed -> @closed_account_error
      :open -> acct.balance
    end
  end

  @doc """
  Update the account's balance by adding the given amount which may be negative.
  """
  @spec update(account, integer) :: any
  def update(account, amount) do
    # Push all the work into the Agent, for concurrency.
    # Note to self: fun returns {"get" thing, new state}
    Agent.get_and_update(account, fn acct ->
      case acct.status do
        :closed -> {@closed_account_error, acct}
        _ -> {:ok, Map.update!(acct, :balance, &(&1 + amount))}
      end
    end)
  end
end
