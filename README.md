# Erlang Banking Simulation Project

This project simulates a banking system with concurrent processes in Erlang.

## Project Structure

The project has the following structure:

- `money.erl`: Master process.
- `customer.erl`: Customer processes.
- `bank.erl`: Bank processes.

## Usage

### 1. Clone the Repository

```bash
git clone https://github.com/yourusername/erlang-banking-simulation.git
cd erlang-banking-simulation
```
### 2. Compile the Erlang Files
```bash
erlc money.erl customer.erl bank.erl
```
### 3. Running the Simulation
  To run the simulation, use the following command:
  ```bash
  erl -noshell -run money start customer_file_name.txt bank_file_name.txt -s init stop
  ```
Replace customer_file_name.txt and bank_file_name.txt with the actual file names containing customer and bank information. Ensure these files are present in the project directory.

For example:
```bash
erl -noshell -run money start customers.txt banks.txt -s init stop
```

