run_simulation <- function(simulation)
{
  assert_erahumed_simulation(simulation)

  simulation |>
    compute_inp() |>
    compute_hbl() |>
    compute_hbc() |>
    compute_hbd() |>
    compute_ca() |>
    compute_ctc() |>
    compute_ctd() |>
    compute_ctl() |>
    compute_rc() |>
    compute_rd() |>
    compute_rl()
}
