# color table for good cluster visualization
color_table = c("black", "pink", "green", "yellow", "blue", "red", 
                "gray", "brown","cyan", "palegreen", "yellowgreen", 
                "violetred4", "thistle", "paleturquoise", "lightyellow",
                "aliceblue", "aquamarine1", "darkgoldenrod", "dimgray",
                "midnightblue")

dis <- function(p1, p2) { 
  sqrt((p1[[1]] - p2[[1]])^2 + (p1[[2]] - p2[[2]])^2) 
}

build_dis_matrix <- function(points) {
  apply(points, 1, function(x) apply(points, 1, function(y) dis(x, y)))
}

calc_density <- function(distances, thresh) {
  help <- function(dist_row) {
    length(dist_row[sapply(dist_row, function(x) x <= thresh)])
  }
  apply(distances, 1, help)
}

calc_average_cover <- function(distances, thresh) {
  mean(calc_density(distances, thresh));
}

## auto tune for the right distance threshold using binary search
binary_search_thresh <- function(distances, lo, hi) {
  if (hi < lo) {
    return -1
  } else {
    mid = (lo + hi) / 2
    sz = nrow(distances)
    cover_num = calc_average_cover(distances, mid)
    if(cover_num >= 0.016 * sz && cover_num <= 0.020 * sz){
      mid
    } else if(cover_num > 0.02 * sz) {
      binary_search_thresh(distances, lo, mid)
    } else binary_search_thresh(distances, mid, hi)
  }
}

calc_density_delta <- function(dist, t) {
  density = calc_density(dist, t)
  max_density = max(density)
  sz = nrow(dist)
  delta = c(1:sz)
  
  f <- function(i) {
    if(density[i] == max_density) delta[i] = max(dist[i, ])
    else delta[i] = min(dist[i, density > density[i]])
  }
  
  delta = sapply(1:sz, FUN = f)
  data.frame(density * delta, density, delta, 1:sz)
}

find_density_cluster_center <- function (d_and_d) {
  tmp = d_and_d$density * d_and_d$delta
  dd_mean = mean(tmp)
  dd_sd = sd(tmp)

  ret = c();
  for(i in 1:nrow(d_and_d)) {
    if(tmp[i] > dd_mean + dd_sd) ret = c(ret, i);
  }
  ret
}

clustering_centers <- function(points, dist, centers, thresh) {
  sz = length(centers);
  ret = rep(0, sz)
  curr = 2;
  set = rep(FALSE, sz)
  d = dist[centers[1], centers]
  set[1] = TRUE; ret[1] = 2
  
  while(any(set == FALSE)) {
    idx = -1; node = -1;
    tmp_dist = 10e6
    for(i in 1:sz) {
      if(!set[i] && d[i] < tmp_dist) {
        idx = i
        tmp_dist = d[i]
      }
    }
    for(i in 1:sz) {
      if(set[i] && dist[centers[i], centers[idx]] == tmp_dist) {
        node = i
      }
    }

    if(tmp_dist < thresh) {ret[idx] = ret[node]}
    else { ret[idx] = curr + 1; curr = curr + 1;}
    set[idx] = TRUE
    d = mapply(min, d, dist[centers[idx], centers])
  }
  ret
}

density_peak_cluster <- function(datas) {
  points = datas[,1:2]
  sz = nrow(points)
  distances = build_dis_matrix(points)
  thresh = binary_search_thresh(distances, 0, mean(distances))
  d_and_d = calc_density_delta(distances, thresh)
  density = d_and_d$density; delta = d_and_d$delta
  center = find_density_cluster_center(d_and_d)
  center_type = clustering_centers(points, distances, center, thresh)
  ret = rep(1, sz)
  set = rep(FALSE, sz)
  
  for(i in 1:length(center)) ret[center[i]] = center_type[i]
  scatter_ret = ret

  stack = center; max_dist = max(distances)
  
  dists = rep(max_dist, sz)
  for(i in center) dists[i] = 0
  
  while(any(set == FALSE)) {
    dist = max_dist; idx = -1; type = -1;
    for(j in 1:sz) {
      if(set[j] == FALSE && dists[j] < dist) {
        dist = dists[j];
        idx = j;
      }
    }
    
    dist = max_dist
    for(j in 1:sz) {
      if(ret[j] != 1 && distances[idx, j] < dist) {
        dist = distances[idx, j]
        type = ret[j]
      }
    }
    ret[idx] = type
    set[idx] = TRUE
    dists = mapply(min, dists, distances[idx,])
  }
  
  #find out outlier
  tmp = sort(density)
  density_thresh = tmp[floor(0.01 * sz)]
  density_thresh_mean = mean(tmp)
  tmp = sort(delta)
  delta_thresh = tmp[floor(0.9 * sz)]
  delta_thresh_ten_percent = tmp[floor(0.1 * sz)]
  

  outliers = sapply(1:sz, function(idx) 
      density[idx] <= density_thresh && delta[idx] >= delta_thresh)
  ret[outliers] = 1
  
  # combine near cluster
  combin_cluster <- function() {
    for(i in 1:sz) {
      dist = 10e6; idx = -1;
      for(j in 1:sz) {
        if(ret[j] != ret[i] && distances[i,j] < dist) {
          dist = distances[i,j]
          idx = j
        }
      }
      if(dist <= thresh ) {
        cnt1 = length(distances[i, distances[i,] <= thresh & ret == ret[i]])
        cnt2 = length(distances[idx, distances[idx,] <= thresh & 
                                  ret == ret[idx]])
        if(cnt1 >= density_thresh_mean * 0.6 && 
             cnt2 >= density_thresh_mean * 0.6){
          ret[ret == ret[i]] = ret[idx]
        }
      }
    }
    ret
  }
  
  # plot out the result
  layout(matrix(c(1,2,1,3), 2, 2, byrow = TRUE)) 
  plot(delta ~ density, col=scatter_ret, main = "density and delta scatter")
  ret = combin_cluster()
  ret = sapply(ret, function(x) color_table[x])
  plot(points, col=datas[,3], main = "gold standard")
  plot(points, col=ret, main = "density peak cluster result")
  ret
}

run <- function(fname) {
  data = read.table(fname, sep="\t", header = FALSE)
  density_peak_cluster(data)
}
