import React, { useState } from "react";
import Calendar from "react-calendar";
import "react-calendar/dist/Calendar.css";

const IapetusCalendar = ({ onChange }) => {
  const [date, setDate] = useState(new Date());

  // This handler is trivial for now. It might do more in the future.
  const handleDateChange = (newDate) => {
    setDate(newDate);
    onChange(newDate);
  };

  return (
      <div>
        <Calendar
            value={date}
            onChange={handleDateChange}
            minDate={new Date(2020, 0, 1)}
            maxDate={new Date(2039, 11, 31)}
            showNeighboringMonth={false}
        />
        <p>Selected date: {date.toDateString()}</p>
      </div>
  );
};

export default IapetusCalendar;
