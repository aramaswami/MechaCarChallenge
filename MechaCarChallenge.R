mechaCarmpg_data <- read.csv('MechaCar_mpg.csv')
lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD,data=mechaCarmpg_data)
summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD,data=mechaCarmpg_data))

psi_data <- read.csv('Suspension_Coil.csv')
summarize_lot_psi <- psi_data %>% group_by(Manufacturing_Lot) %>% summarize(mean_psi=mean(PSI), median_psi=median(PSI),variance_psi=var(PSI),sd_psi=sd(PSI))
plt_c <- ggplot(summarize_lot_psi,aes(x=Manufacturing_Lot,y=mean_psi))
plt_c + geom_col()

summarize_all_psi <- psi_data %>% summarize(mean_psi=mean(PSI), median_psi=median(PSI),variance_psi=var(PSI),sd_psi=sd(PSI))

sample_psi <- psi_data %>% sample_n(50)
t.test(sample_psi$PSI,mu=mean(psi_data$PSI))

sample2_psi <- psi_data %>% sample_n(50)
t.test(sample_psi$PSI,sample2_psi$PSI)
