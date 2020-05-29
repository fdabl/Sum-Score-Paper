library('scales')
library('mvtnorm')
library('grDevices')
library('RColorBrewer')
set.seed(1)

n <- 100
lab <- 0.80
cols <- brewer.pal(4, 'Set1')
cols2 <- brewer.pal(6, 'Set1')

S <- cbind(c(1, 0), c(0, 1))

d1 <- rbind(
  rmvnorm(n, c(2, 2), S),
  rmvnorm(n, c(3, 3), S),
  rmvnorm(n, c(3.5, 3), S)
)

d2 <- rbind(
  rmvnorm(n, c(5, 4.5), S),
  rmvnorm(n, c(6, 5.5), S),
  rmvnorm(n, c(7, 8), S)
)

d <- cbind(rbind(d1, d2), rep(c(0, 1), each = 3*n))


pdf('Figures/Figure-1.pdf', width = 8, height = 3.5)
par(mar = c(3.5, 4.1, 3.1, 2.1))
par(mfrow = c(1, 2))

plot(
  d, pch = 20, xlim = c(-0, 10), ylim = c(-0, 10), axes = FALSE, col = 'gray40',
  xlab = '', ylab = '', main = 'Marginal Relationship', cex = 0.50, font.main = 1,
  xaxs = 'i', yaxs = 'i' # axes touch at 0
)

axis(1, cex.axis = lab)
axis(2, las = 2, cex.axis = lab)
abline(lm(d[, 2] ~ d[, 1]), lwd = 1.5, lty = 2, col = 'gray40')
mtext(expression(X[1]), side = 1, line = 2.25, cex = 1)
mtext(expression(X[2]), side = 2, line = 2.25, cex = 1)
cor(d[, 2], d[, 1])

cutoff <- 8.50
group <- apply(d[, 1:2], 1, sum) < cutoff

plot(
  d, pch = 20, xlim = c(-0, 10), ylim = c(-0, 10),
  axes = FALSE, cex = 0.50, col = cols[ifelse(group, 2, 1)],
  xlab = '', ylab = '', main = 'Splitting on Sum-score', font.main = 1,
  xaxs = 'i', yaxs = 'i' # axes touch at 0
)

axis(1, cex.axis = lab)
axis(2, las = 2, cex.axis = lab)
text(5.9, 0.50, 'No Diagnosis', cex = 0.75)
text(1.50, 9.50, 'Diagnosis', cex = 0.75)

d1z <- d[group, ]
d2z <- d[!group, ]

abline(a = cutoff, b = -1, lty = 1, lwd = 2)
abline(lm(d1z[, 2] ~ d1z[, 1]), lwd = 1.5, lty = 2, col = cols[2])
abline(lm(d2z[, 2] ~ d2z[, 1]), lwd = 1.5, lty = 2, col = cols[1])
mtext(expression(X[1]), side = 1, line = 2.25, cex = 1)
mtext(expression(X[2]), side = 2, line = 2.25, cex = 1)

cor(d1z[, 1], d1z[, 2])
cor(d2z[, 1], d2z[, 2])
dev.off()


pdf('Figures/Figure-2.pdf', width = 8, height = 3.5)
par(mar = c(3.5, 4.1, 3.1, 2.1))
par(mfrow = c(1, 2))

plot(
  d, pch = 20, xlim = c(-0, 10), ylim = c(-0, 10), axes = FALSE, col = 'gray40',
  xlab = '', ylab = '', main = 'Marginal Relationship', cex = 0.50, font.main = 1,
  xaxs = 'i', yaxs = 'i' # axes touch at 0
)

axis(1, cex.axis = lab)
axis(2, las = 2, cex.axis = lab)
mtext(expression(X[1]), side = 1, line = 2.25, cex = 1)
mtext(expression(X[2]), side = 2, line = 2.25, cex = 1)

abline(lm(d[, 2] ~ d[, 1]), lwd = 1.5, lty = 2, col = 'gray40')
colors <- c(rep(cols[3], 300), rep(cols[4], 300))

group2 <- apply(d, 1, function(x) { x[2] < (log(x[1]) + 3)})
plot(
  d, pch = 20, xlim = c(-0, 10), ylim = c(-0, 10),
  col = ifelse(group2, cols[3], cols[4]), axes = FALSE, cex = .50,
  xlab = '', ylab = '', main = 'Conditioning on Group', font.main = 1,
  xaxs = 'i', yaxs = 'i' # axes touch at 0
)

axis(1, cex.axis = lab)
axis(2, las = 2, cex.axis = lab)
mtext(expression(X[1]), side = 1, line = 2.25, cex = 1)
mtext(expression(X[2]), side = 2, line = 2.25, cex = 1)

d1zz <- d[group2, ]
d2zz <- d[!group2, ]

abline(a = cutoff, b = -1, lty = 1, lwd = 2)
abline(lm(d1zz[, 2] ~ d1zz[, 1]), lwd = 1.5, lty = 2, col = cols[3])
abline(lm(d2zz[, 2] ~ d2zz[, 1]), lwd = 1.5, lty = 2, col = cols[4])

text(5.9, 0.50, 'No Diagnosis', cex = 0.75)
text(1.50, 9.50, 'Diagnosis', cex = 0.75)
legend(
  x = 8, y = 3, legend = c('G = 1', 'G = 0'),
  col = cols[c(4, 3)], bty = 'n', pch = 20, cex = 0.75
)

cor(d1zz[, 1], d1zz[, 2])
cor(d2zz[, 1], d2zz[, 2])
dev.off()


pdf('Figures/Figure-3.pdf', width = 8, height = 3.5)
par(mar = c(5.1, 4.1, 4.1, 2.1))

set.seed(1)
n <- 600
S <- cbind(c(1, .75), c(.75, 1))
d <- rmvnorm(n, c(4, 4), S)
cols <- brewer.pal(4, 'Set1')

plot_groups <- function(d, cutoff, main = '') {
  group <- apply(d[, 1:2], 1, sum) < cutoff
  
  plot(
    d, pch = 20, xlim = c(0, 10), ylim = c(0, 10),
    axes = FALSE, cex = 0.35, col = cols[ifelse(group, 2, 1)],
    xlab = '', ylab = '', main = main, font.main = 1 ,
    xaxs = 'i', yaxs = 'i' # axes touch at 0
  )
  
  mtext(expression(X[1]), side = 1, line = 2.25, cex = 1)
  mtext(expression(X[2]), side = 2, line = 2.25, cex = 1)
  axis(1, cex.axis = 0.80)
  axis(2, las = 2, cex.axis = 0.80)
  abline(a = cutoff, b = -1, lty = 1, lwd = 1.5)
  legend(
    'topright', legend = c('High Group', 'Low Group'),
    col = cols[1:2], bty = 'n', pch = 20, cex = 0.75
  )
  
  d1z <- d[group, ]
  d2z <- d[!group, ]
  m <- lm(d[, 2] ~ d[, 1])
  m1 <-lm(d1z[, 2] ~ d1z[, 1])
  m2 <- lm(d2z[, 2] ~ d2z[, 1])
  abline(m1, lwd = 1.5, lty = 2, col = cols[2])
  abline(m2, lwd = 1.5, lty = 2, col = cols[1])
}

par(mfrow = c(1, 2))
par(mar = c(3.5, 4.1, 3.1, 2.1))
plot_groups(d, cutoff = 6, main = 'Low Cut-Off')
plot_groups(d, cutoff = 9.5, main = 'High Cut-Off')
dev.off()
