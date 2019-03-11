return {
    anniversary = function (epoch_time, date_format)
        assert(type(epoch_time) == "number")
        date_format = date_format or '%x'
        return os.date(date_format, epoch_time + 1e9)
    end,
}
