
% mnesia tables
-record(keys, {key, id, last_date, sex, age}).
-record(positions, {snapshot_id, snapshot_square, snapshot, id, lat, lon, square, sex, age, filter, pic, msg, json_prep}).
% -record(positions, {snapshot_id, snapshot_square, snapshot, id, lat, lon, square, sex = "n", age = "n", filter = "n", pic = "n", msg = "n", json_prep}).
-record(cache, {snapshot_square, snapshot, json}).
-record(snaps, {key, snap_write, snap_read, snap_cache}).

% ets protected
-record(server_inf, {name, value}).
-record(sock, {name, lat_min, lat_max, lon_min, lon_max, url}).
-record(zone, {name, sock_name, lat_min, lat_max, lon_min, lon_max}).
-record(live, {name, modified, content}).
