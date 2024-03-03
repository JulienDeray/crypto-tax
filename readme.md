# Crypto tax

Given an account statement in CSV format, computes how much of each crypto can be sold without having to pay taxes in Portugal (i.e. bought > 365 days ago).

## Usage

```
/opt/homebrew/bin/scala-cli crypto-tax.sc -- csv_file.csv [currency]
```

## CSV format

```csv
Local time,Time in UTC,Type,Currency,Gross amount,Gross amount (EUR),Fee,Fee (EUR),Net amount,Net amount (EUR),Note
2020-09-24 17:52:58,2020-09-24 16:52:58,Deposit,GBP,50,54.66,0,0,50,54.66,
2020-09-24 18:07:44,2020-09-24 17:07:44,Sell,GBP,10,10.86,0,0,10,10.86,Exchanged to 161.683 CHSB. The fees for the exchange was taken in the bought currency
```